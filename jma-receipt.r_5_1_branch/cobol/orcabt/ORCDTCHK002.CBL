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
       PROGRAM-ID.                     ORCDTCHK002.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 対象患者抽出
      *  管理者            :
      *  作成日付    作業者        記述
      *  03/11/21    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.12    NACL-太田    04/08/09  院内処方の包括入力はチェック
      *                                     の対象外とする
      *  03.05.00    NACL-太田    07/06/04  グループ診療対応
      *  04.01.00    NACL-太田    08/01/21  チェックマスタ変更対応
      *
      *  04.06.01    NACL-門脇    13/02/18  労災レセ電対応
      *                                     （新診療行為コード対応）
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *    データチェック診療会計
           SELECT  DCACCT-FILE     ASSIGN  DTCHKACCTPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DCACCT-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック診療行為
           SELECT  DCSRY-FILE      ASSIGN  DTCHKSRYPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DCSRY-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック算定履歴
           SELECT  DCSANTEI-FILE
                                   ASSIGN  DTCHKSANTEIPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DCSANTEI-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *
      *    データチェック中間ファイル（対象患者）
           SELECT  DTCHK003-FILE   ASSIGN  DTCHK003PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（チェック結果）
           SELECT  DTCHK004-FILE
                                   ASSIGN  DTCHK004PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK004-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（チェックマスタに存在しない診療行為）
           SELECT  DTCHK008-FILE   ASSIGN  DTCHK008PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK008-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（複数病名登録チェック）
           SELECT  DTCHK009-FILE   ASSIGN  DTCHK009PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK009-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（チェックマスタ２）
           SELECT  DTCHK010-FILE   ASSIGN  DTCHK010PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK010-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（チェック例外マスタ）
           SELECT  DTCHK011-FILE   ASSIGN  DTCHK011PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK011-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（病名）
           SELECT  DTCHK012-FILE   ASSIGN  DTCHK012PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK012-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（算定回数）
           SELECT  DTCHK013-FILE
                                   ASSIGN  DTCHK013PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK013-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（チェックマスタ）
           SELECT  DTCHK018-FILE
                                   ASSIGN  DTCHK018PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK018-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（コメント関連）
           SELECT  DTCHK022-FILE
                                   ASSIGN  DTCHK022PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK022-ISAM-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    エラーファイル
           SELECT  DTCHKERR-FILE   ASSIGN  DTCHKERRPARA
                                   FILE    STATUS  IS  STS-DTCHKERR.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *    データチェック診療会計
       FD  DCACCT-FILE.
       01  DCACCT-REC.
           COPY    "CPDCACCT.INC".
      *
      *    データチェック診療行為
       FD  DCSRY-FILE.
       01  DCSRY-REC.
           COPY    "CPDCSRY.INC".
      *
      *    データチェック算定履歴
       FD  DCSANTEI-FILE.
       01  DCSANTEI-REC.
           COPY    "CPSANTEI.INC"  REPLACING  //SANTEI-//
                                   BY         //DCSANTEI-//.
      *
      *    データチェック中間ファイル（対象患者情報）
       FD  DTCHK003-FILE.
           COPY    "CPDTCHK001.INC"
                                   REPLACING  //DTCHK001//
                                   BY         //DTCHK003//.
      *
      *    データチェック中間ファイル（チェック結果）
       FD  DTCHK004-FILE.
           COPY    "CPDTCHK004.INC".
      *
      *    データチェック中間ファイル（チェックマスタに存在しない診療行為）
       FD  DTCHK008-FILE.
           COPY    "CPDTCHK008.INC".
      *
      *    データチェック中間ファイル（複数病名登録チェック）
       FD  DTCHK009-FILE.
           COPY    "CPDTCHK009.INC".
      *
      *    データチェック中間ファイル（チェックマスタ２）
       FD  DTCHK010-FILE.
       01  DTCHK010-REC.
         02 DTCHK010-SET-G.
           03  DTCHK010-SETCD-GET              PIC 9(01).
           03  DTCHK010-SETCD-DAY-OCC          OCCURS  31.
               05  DTCHK010-SETCD-MAX          PIC 9(05).
               05  DTCHK010-SETCD-OCC          OCCURS  100.
                   07  DTCHK010-SETCD1         PIC X(09).
                   07  DTCHK010-SETCD2         PIC X(09).
         02    DTCHK010-DAT.
           COPY    "CPCHKSND.INC"  REPLACING  //CHKSND//
                                   BY         //DTCHK010//.
      *
      *    データチェック中間ファイル（チェック例外マスタ）
       FD  DTCHK011-FILE.
           COPY    "CPCHKREIGAI.INC"  REPLACING  //CHKREIGAI//
                                      BY         //DTCHK011//.
      *
      *    データチェック中間ファイル（病名）
       FD  DTCHK012-FILE.
           COPY    "CPORCSS012.INC"   REPLACING  //SS012//
                                      BY         //DTCHK012//.
      *
      *    データチェック中間ファイル（算定回数）
       FD  DTCHK013-FILE.
           COPY    "CPDTCHK013.INC".
      *
      *    データチェック中間ファイル（チェックマスタ）
       FD  DTCHK018-FILE.
       01  DTCHK018-REC.
           COPY    "CPCHK.INC"      REPLACING  //CHK-//
                                    BY         //DTCHK018-//.
      *
      *    データチェック中間ファイル（コメント関連）
       FD  DTCHK022-FILE.
       01  DTCHK022-REC.
         02    DTCHK022-ISAM-KEY.
           03  DTCHK022-ISAM-SRYCD     PIC X(09).
           03  DTCHK022-ISAM-ITEMNO    PIC 9(04).
           03  DTCHK022-ISAM-EDANO     PIC 9(02).
           03  DTCHK022-ISAM-RENNUM    PIC 9(02).
         02    DTCHK022-RKISAI-AREA.
           COPY    "CPRECEKISAI.INC"   REPLACING  //RKISAI-//
                                       BY         //DTCHK022-//.
      *
      *    エラーファイル
       FD  DTCHKERR-FILE.
       01  DTCHKERR-REC                PIC X(200). 
      *
       WORKING-STORAGE                 SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
           COPY    "HCM401.INC".
      *
      *    エラーファイルファイル名称領域 
           COPY    "CPRECEERR.INC"   REPLACING  //RECEERR//
                                   BY         //DTCHKERRPARA//.
      *
      *    中間ファイルファイル名称領域 
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK003PARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK004PARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHKACCTPARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK008PARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK009PARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK010PARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK011PARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK012PARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK013PARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK018PARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK022PARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHKSRYPARA//.
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHKSANTEIPARA//.
           COPY    "CPASGNPARA.INC".
      *
      *    データチェック共通コピー句
           COPY    "CPDTCHKCOMMON.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-COMMON                              PIC X(02).
           03  STS-DTCHKERR                            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ERR                                 PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-HKNNUM                              PIC 9(01).
           03  FLG-DCACCT                              PIC 9(01).
           03  FLG-DCSRY                               PIC 9(01).
           03  FLG-DCSANTEI                            PIC 9(01).
           03  FLG-PTHKNINF                            PIC 9(01).
           03  FLG-PTKOHINF                            PIC 9(01).
           03  FLG-PTINF                               PIC 9(01).
           03  FLG-PTNUM                               PIC 9(01).
           03  FLG-CHKMST                              PIC 9(01).
           03  FLG-CHKSND                              PIC 9(01).
           03  FLG-CHKREIGAI                           PIC 9(01).
           03  FLG-MATERK                              PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-PTBYOMEI                            PIC 9(01).
           03  FLG-NYUINACCT                           PIC 9(01).
           03  FLG-MONTHLYNUM                          PIC 9(01).
           03  FLG-PTCOM                               PIC 9(01).
           03  FLG-PTRSIINF                            PIC 9(01).
           03  FLG-LABOR-SIO                           PIC 9(01).
           03  FLG-DTCHK003                            PIC 9(01).
           03  FLG-DTCHK008                            PIC 9(01).
           03  FLG-DTCHK009                            PIC 9(01).
           03  FLG-DTCHK010                            PIC 9(01).
           03  FLG-DTCHK011                            PIC 9(01).
           03  FLG-DTCHK012                            PIC 9(01).
           03  FLG-DTCHK013                            PIC 9(01).
           03  FLG-DTCHK018                            PIC 9(01).
           03  FLG-DTCHK022                            PIC 9(01).
           03  FLG-YKZBYO                              PIC 9(01).
           03  FLG-SRYBYO                              PIC 9(01).
           03  FLG-YKZHEI                              PIC 9(01).
           03  FLG-SRYHEI                              PIC 9(01).
           03  FLG-BYOYKZ                              PIC 9(01).
           03  FLG-BYOSRY                              PIC 9(01).
           03  FLG-YKZKNKBYO                           PIC 9(01).
           03  FLG-SRYSNT                              PIC 9(01).
           03  FLG-SRYHEIA                             PIC 9(01).
           03  FLG-SRYHEIB                             PIC 9(01).
           03  FLG-CHKEND-G.
               05  FLG-CHKEND                          PIC 9(01)
                                                       OCCURS  50.
           03  FLG-JIHI                                PIC 9(01).
           03  FLG-RSI-JBI                             PIC 9(01).
           03  FLG-KOUGAI                              PIC 9(01).
           03  FLG-TAISYO-HKN                          PIC 9(01).
           03  FLG-TAISYO-SRY                          PIC 9(01).
           03  FLG-INGAISYOHOU                         PIC 9(01).
           03  FLG-INGAIZAI                            PIC 9(01).
           03  FLG-LOOP-END                            PIC 9(01).
           03  FLG-TSRYKA-OVER                         PIC 9(01).
           03  FLG-TKAISU-OVER                         PIC 9(01).
           03  FLG-SHOSAISIN                           PIC 9(01).
           03  FLG-KANACHK                             PIC S9(09).
           03  FLG-PTBYO-DAY-G.
               05  FLG-PTBYO-DAY                       PIC 9(01)
                                                       OCCURS  31.
           03  FLG-PTBYO-DAY-KA-G.
               05  FLG-PTBYO-DAY-KA-OCC                OCCURS  30.
                   07  FLG-PTBYO-DAY-KA                PIC 9(01)
                                                       OCCURS  31.
           03  FLG-PTBYO-DAY2-G.
               05  FLG-PTBYO-DAY2                      PIC 9(01)
                                                       OCCURS  31.
           03  FLG-PTBYO-DAY2-KA-G.
               05  FLG-PTBYO-DAY2-KA-OCC               OCCURS  30.
                   07  FLG-PTBYO-DAY2-KA               PIC 9(01)
                                                       OCCURS  31.
           03  FLG-SYOSIN-ERR-DAY-G.
               05  FLG-SYOSIN-ERR-DAY                  PIC 9(01)
                                                       OCCURS  31.
           03  FLG-SAISIN-ERR-DAY-G.
               05  FLG-SAISIN-ERR-DAY                  PIC 9(01)
                                                       OCCURS  31.
           03  FLG-SRYHEI-DAY-G.
               05  FLG-SRYHEI1-DAY-G.
                   07  FLG-SRYHEI1-DAY                 PIC 9(01)
                                                       OCCURS  31.
               05  FLG-SRYHEI2-DAY-G.
                   07  FLG-SRYHEI2-DAY                 PIC 9(01)
                                                       OCCURS  31.
           03  FLG-SRYHEI-DAY-R  REDEFINES FLG-SRYHEI-DAY-G.
               05  FLG-SRYHEI-DAY-OCC                  OCCURS  2.
                   07  FLG-SRYHEI-DAY                  PIC 9(01)
                                                       OCCURS  31.
           03  FLG-PTBYO-TAISYO                        PIC 9(01).
           03  FLG-PTBYO-ERR                           PIC 9(01).
           03  FLG-PTBYO-MOJI                          PIC 9(01).
           03  FLG-CHK-END                             PIC 9(01).
           03  FLG-PTBYO-LEN-ERR                       PIC 9(01).
           03  FLG-UTAGAI                              PIC 9(01).
           03  FLG-KYUSEI                              PIC 9(01).
           03  FLG-BYOMEI-ERR                          PIC 9(01).
           03  FLG-CHKMST-SRY                          PIC 9(01).
           03  FLG-HIT                                 PIC 9(01).
           03  FLG-SRYKA                               PIC 9(01).
           03  FLG-INTERACT                            PIC 9(01).
           03  FLG-JNISSU-HIT                          PIC 9(01).
           03  FLG-SEIHO                               PIC 9(01).
           03  FLG-RJN                                 PIC 9(01).
           03  FLG-PROC                                PIC 9(01).
           03  FLG-THEIYOU-SET                         PIC 9(01).
           03  FLG-RECEDEN-CHK                         PIC 9(01).
           03  FLG-RSI-RECEDEN-CHK                     PIC 9(01).
           03  FLG-SANTEI                              PIC 9(01).
           03  FLG-NBFUKACD                            PIC 9(01).
           03  FLG-NBFUKATG                            PIC 9(01).
           03  FLG-TAISYO-BYOMEI                       PIC 9(01).
           03  FLG-KOHERR                              PIC 9(01).
           03  FLG-SRYSNT-G.
               05  FLG-SRYSNT-DAYCHK                   PIC 9(01).
               05  FLG-SRYSNT-MONTHCHK                 PIC 9(01).
               05  FLG-SRYSNT-DAYERR                   PIC 9(01)
                                                       OCCURS  31.
               05  FLG-SRYSNT-MONTHERR                 PIC 9(01).
           03  FLG-HKNKIGOERR                          PIC 9(01).
           03  FLG-HKNNUMERR                           PIC 9(01).
           03  FLG-CHK004                              PIC 9(01).
           03  FLG-INNAI-DAY-G.
               05  FLG-INNAI-DAY-OCC                   OCCURS 2.
                   07  FLG-INNAI-DAY                   PIC 9(01)
                                                       OCCURS  31.
           03  FLG-INGAI-DAY-G.
               05  FLG-INGAI-DAY-OCC                   OCCURS 2.
                   07  FLG-INGAI-DAY                   PIC 9(01)
                                                       OCCURS  31.
           03  FLG-DATACHECK                           PIC 9(01).
           03  FLG-KIKANCHKCD                          PIC 9(01).
           03  FLG-REIGAI                              PIC 9(01).
           03  FLG-TARGET                              PIC 9(01).
           03  FLG-COMBERR                             PIC 9(01).
           03  FLG-TENSUPLUS2                          PIC 9(01).
           03  FLG-INPUTCD                             PIC 9(01).
           03  FLG-INPUTSET                            PIC 9(01).
           03  FLG-SRYCDCHG                            PIC 9(01).
           03  FLG-COMERR                              PIC 9(01).
           03  FLG-ZAIARI                              PIC 9(01).
           03  FLG-AGE70ERR                            PIC 9(01).
           03  FLG-CHKSKIP                             PIC 9(01).
           03  FLG-TMPSND                              PIC 9(01).
           03  FLG-TEKBYO                              PIC 9(01).
           03  FLG-PTCRNIN                             PIC 9(01).
           03  FLG-HAISHI                              PIC 9(01).
           03  FLG-RSI-KANA                            PIC 9(01).
           03  FLG-NORMAL                              PIC 9(01).
           03  FLG-TANKI-SHOBYO                        PIC 9(01).
           03  FLG-KAISU-CHK                           PIC 9(01).
           03  FLG-KAIGO-CHK                           PIC 9(01).
           03  FLG-HKNKBN                              PIC 9(01).
           03  FLG-TEKDIS                              PIC 9(01).
           03  FLG-DAY                                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-DTCHK003                            PIC 9(06).
           03  CNT-DTCHK004                            PIC 9(06).
           03  CNT-JNISSU                              PIC 9(06).
           03  CNT-SRYYMD                              PIC 9(06).
           03  CNT-NISSU                               PIC 9(06).
           03  CNT-CHKERR                              PIC 9(06).
      *    医療機関コード、患者ＩＤ、診療科で抽出した件数
           03  CNT-PTBYOMEI1                           PIC 9(06).
      *    CNT-PTBYOMEI1から、さらに入外区分、適用期間で絞った件数
           03  CNT-PTBYOMEI2                           PIC 9(06).
           03  CNT-HKNTG                               PIC 9(06).
           03  CNT-PTSYUBYO                            PIC 9(06).
           03  CNT-BYOMEIMOJI                          PIC 9(06).
           03  CNT-TAISYO                              PIC 9(06).
           03  CNT-STR                                 PIC 9(06).
           03  CNT-GAIHAKU-ERR                         PIC 9(06).
           03  CNT-CHIHOU                              PIC 9(06).
           03  CNT-UNCHIHOU                            PIC 9(06).
           03  CNT-CHKSND                              PIC 9(06).
           03  CNT-COMMENT                             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                                PIC 9(08).
           03  SYS-YMD.
               05  SYS-YY                              PIC 9(02).
               05  SYS-MM                              PIC 9(02).
               05  SYS-DD                              PIC 9(02).
      *
       01  IDX-AREA.
           03  IDX0                                    PIC 9(05).
           03  IDX1                                    PIC 9(05).
           03  IDX2                                    PIC 9(05).
           03  IDX3                                    PIC 9(05).
           03  IDX4                                    PIC 9(05).
           03  IDX5                                    PIC 9(05).
           03  IDX6                                    PIC 9(05).
           03  IDX7                                    PIC 9(05).
           03  IDX8                                    PIC 9(05).
           03  IDX9                                    PIC 9(05).
           03  IDX10                                   PIC 9(05).
           03  IDX11                                   PIC 9(05).
           03  IDX12                                   PIC 9(05).
           03  IDX13                                   PIC 9(05).
           03  IDX14                                   PIC 9(05).
           03  IDX15                                   PIC 9(05).
           03  IDX16                                   PIC 9(05).
           03  IDX17                                   PIC 9(05).
           03  IDX18                                   PIC 9(05).
           03  IDX19                                   PIC 9(05).
           03  IDX20                                   PIC 9(05).
           03  IDX21                                   PIC 9(05).
           03  IDX22                                   PIC 9(05).
           03  IDX23                                   PIC 9(05).
           03  IDX24                                   PIC 9(05).
           03  IDX25                                   PIC 9(05).
           03  IDX26                                   PIC 9(05).
           03  IDX27                                   PIC 9(05).
           03  IDX28                                   PIC 9(05).
           03  IDX29                                   PIC 9(05).
           03  IDX30                                   PIC 9(05).
           03  IDX31                                   PIC 9(05).
           03  IDX32                                   PIC 9(05).
           03  IDX33                                   PIC 9(05).
           03  IDX34                                   PIC 9(05).
           03  IDX35                                   PIC 9(05).
           03  IDX36                                   PIC 9(05).
           03  IDX37                                   PIC 9(05).
           03  IDX38                                   PIC 9(05).
           03  IDX39                                   PIC 9(05).
           03  IDX40                                   PIC 9(05).
           03  IDX41                                   PIC 9(05).
           03  IDX42                                   PIC 9(05).
           03  IDXA                                    PIC 9(05).
           03  IDXD                                    PIC 9(05).
           03  IDXE                                    PIC 9(05).
           03  IDXH                                    PIC 9(05).
           03  IDXI                                    PIC 9(05).
           03  IDXJ                                    PIC 9(05).
           03  IDXK                                    PIC 9(05).
           03  IDXN                                    PIC 9(05).
           03  IDXP                                    PIC 9(05).
           03  IDXR                                    PIC 9(05).
           03  IDXS                                    PIC 9(05).
           03  IDX-TSRYKA                              PIC 9(05).
           03  IDX-TKAISU                              PIC 9(05).
           03  IDX-ERRCD                               PIC 9(05).
           03  IDX-TSRYCD                              PIC 9(05).
           03  IDX-TTNS                                PIC 9(05).
           03  IDX-COMB                                PIC 9(05).
           03  IDX-DAY                                 PIC 9(02).
           03  IDX-ACCT                                PIC 9(05).
           03  IDX-SRY                                 PIC 9(05).
           03  IDX-ZAI                                 PIC 9(05).
           03  IDX-MAX                                 PIC 9(05).
           03  IDX-KANA                                PIC 9(05).
           03  IDX-CHK                                 PIC 9(05).
           03  IDX-KANRIRYO                            PIC 9(05).
           03  IDX-KGNM                                PIC 9(05).
           03  IDX-TTNSP2                              PIC 9(05).
           03  IDX-PTSRY                               PIC 9(05).
           03  IDXS1                                   PIC 9(05).
           03  IDXS2                                   PIC 9(05).
           03  IDXD1                                   PIC 9(05).
           03  IDXD2                                   PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-SYMD                                PIC X(08).
           03  WRK-EDTYMD1                             PIC X(10).
           03  WRK-EDTYMD3                             PIC X(22).
           03  WRK-MAE-INPUT                           PIC X(300).
           03  WRK-OUT-INPUT                           PIC X(300).
           03  WRK-OUT-MAX                             PIC 9(05).
           03  WRK-ZOGENPTN                            PIC X(01).
           03  WRK-ZOGEN                               PIC S9(05).
           03  WRK-DTCHKERR                            PIC X(200).
           03  WRK-KJNYMD                              PIC X(08).
           03  WRK-TSRYKA-SRYCD-MAX                    PIC 9(05).
           03  WRK-TSRYKA-PSRYCD-MAX                   PIC 9(05).
           03  WRK-TKAISU-SRYCD-MAX                    PIC 9(05).
           03  WRK-SRYCD                               PIC X(09).
           03  WRK-TNS-YMD.
               05  WRK-TNS-YY                          PIC 9(04).
               05  WRK-TNS-MM                          PIC 9(02).
               05  WRK-TNS-DD                          PIC 9(02).
           03  WRK-DAY-Z                               PIC ZZ.
           03  WRK-NYUGAIKBN                           PIC X(01).
           03  WRK-HKNID                               PIC 9(10).
           03  WRK-KOHID                               PIC 9(10).
           03  WRK-ERRCD.
               05  WRK-ERRCD-NUM                       PIC 9(06).
           03  WRK-ERRMES                              PIC X(200).
           03  WRK-ERRMES-PLUS                         PIC X(200).
           03  WRK-PTBYOMES-PLUS                       PIC X(200).
           03  WRK-BYOMEICD                            PIC X(07).
           03  WRK-BYOMEI                              PIC X(200).
           03  WRK-HKNNUM                              PIC X(03).
           03  WRK-PTHKN-KIGO                          PIC X(80).
           03  WRK-PTHKN-NUM                           PIC X(80).
           03  WRK-KANACHK-MAE-INPUT                   PIC X(400).
           03  WRK-PTKOH-FTNJANUM                      PIC X(08).
           03  WRK-PTKOH-JKYSNUM                       PIC X(20).
           03  WRK-HEN-PTBYOMEI                        PIC X(100).
           03  WRK-HEN-PTBYOMEI2                       PIC X(144).
           03  WRK-STR                                 PIC X(500).
           03  WRK-STR-TMP                             PIC X(500).
           03  WRK-PNT                                 PIC 9(05).
           03  WRK-UTAGAI-CHK-YM                       PIC X(06).
           03  WRK-KYUSEI-CHK-YM                       PIC X(06).
           03  WRK-YMD.
               05  WRK-YY                              PIC 9(04).
               05  WRK-MM                              PIC 9(02).
               05  WRK-DD                              PIC 9(02).
           03  WRK-SRYKA                               PIC X(02).
           03  WRK-CHK-CD                              PIC X(09).
           03  WRK-ORCBSD01-CHKKBN                     PIC X(01).
           03  WRK-FLG-CHKEND                          PIC 9(01).
           03  WRK-NYUGAI-MES                          PIC X(10).
           03  WRK-PTINF-NAME                          PIC X(100).
           03  WRK-PTINF-KANANAME                      PIC X(100).
           03  WRK-CHK-CHKKBN                          PIC X(01).
           03  WRK-CHK-CDKBN                           PIC X(01).
           03  WRK-CHK-SRYCD                           PIC X(09).
           03  WRK-SHO                                 PIC 9(05).
           03  WRK-AMARI                               PIC 9(05).
           03  WRK-STR-LEFT                            PIC X(500).
           03  WRK-STR-LEFT-Z      REDEFINES           WRK-STR-LEFT.
               05  WRK-STR-LEFT-ZX99                   PIC Z(9)9.
               05  WRK-STR-LEFT-FL                     PIC X(490).
           03  WRK-STR-LEFT-TMP                        PIC X(500).
           03  WRK-N.
               05  WRK-KNS-N                           PIC X(10).
               05  WRK-TIME-N                          PIC X(16).
           03  WRK-PTBYO-SRYYMD                        PIC X(08).
           03  WRK-HKNCOMBI                            PIC 9(04).
           03  WRK-ACCTKBN                             PIC X(01).
           03  WRK-HKNCOMBIMEI                         PIC X(50).
           03  WRK-COMB-RSI-HKNKBN                     PIC X(01).
           03  WRK-HKNCOMBINUM                         PIC X(50).
           03  WRK-HKNCOMBI-DAY-G.
               05  WRK-HKNCOMBI-DAY                    PIC 9(03)
                                                       OCCURS  31.
           03  WRK-TKAISU-SRYCD                        PIC X(09).
           03  WRK-MAX                                 PIC 9(05).
           03  WRK-KIGONUM-STR                         PIC X(50).
           03  WRK-NISSU-AREA.
               05  WRK-NISSU-DAY-G.
                   07  WRK-NISSU-DAY                   PIC 9(01)
                                                       OCCURS  31.
               05  WRK-NISSU-OT.
                   07  WRK-NISSU-KUGIRI-FLG            PIC 9(01).
                   07  WRK-NISSU-RENZOKU-CNT           PIC 9(03).
                   07  WRK-NISSU-ST-G.
                       09  WRK-NISSU-ST                PIC 9(02).
                       09  WRK-NISSU-ST-X              PIC X(02).
                       09  WRK-NISSU-ST-R1 REDEFINES   WRK-NISSU-ST-X.
                           11  WRK-NISSU-ST-Z9         PIC Z9.
                       09  WRK-NISSU-ST-R2 REDEFINES   WRK-NISSU-ST-X.
                           11  WRK-NISSU-ST-9          PIC 9(01).
                           11  WRK-NISSU-ST-FL         PIC X(01).
                   07  WRK-NISSU-ED-G.
                       09  WRK-NISSU-ED                PIC 9(02).
                       09  WRK-NISSU-ED-X              PIC X(02).
                       09  WRK-NISSU-ED-R1 REDEFINES   WRK-NISSU-ED-X.
                           11  WRK-NISSU-ED-Z9         PIC Z9.
                       09  WRK-NISSU-ED-R2 REDEFINES   WRK-NISSU-ED-X.
                           11  WRK-NISSU-ED-9          PIC 9(01).
                           11  WRK-NISSU-ED-FL         PIC X(01).
                   07  WRK-NISSU-MES                   PIC X(200).
                   07  WRK-NISSU-PTR                   PIC 9(03).
                   07  WRK-NISSU-LASTYMD               PIC X(08).
           03  WRK-SYOSIN-CHK-YMD.
               05  WRK-SYOSIN-CHK-YY                   PIC 9(04).
               05  WRK-SYOSIN-CHK-MM                   PIC 9(02).
               05  WRK-SYOSIN-CHK-DD                   PIC 9(02).
           03  WRK-TNS-NBFUKATEN                       PIC 9(10).
           03  WRK-SNT-NBFUKATEN                       PIC 9(10).
           03  WRK-ACCTNAME                            PIC X(20).
           03  WRK-TENKIYMD                            PIC X(08).
           03  WRK-MAE-SRYYM-G.
               05  WRK-MAE-SRYYM                       PIC X(06).
               05  WRK-MAE-SRYYM-DAY                   PIC 9(01)
                                                       OCCURS  31.
           03  WRK-KON-SRYYM-G.
               05  WRK-KON-SRYYM                       PIC X(06).
               05  WRK-KON-SRYYM-DAY                   PIC 9(01)
                                                       OCCURS  31.
           03  WRK-SYOSINYMD-G.
               05  WRK-SYOSINYMD-MAX                   PIC 9(03).
               05  WRK-SYOSINYMD-OCC                   OCCURS  50.
                   07  WRK-SYOSINYMD.
                       09  WRK-SYOSIN-YM               PIC X(06).
                       09  WRK-SYOSIN-DD               PIC 9(02).
           03  WRK-ZAIHKNNUM                           PIC X(03).
           03  WRK-HKNJANUMLEN                         PIC 9(03).
           03  WRK-SRYYMD                              PIC X(08).
           03  WRK-DTCHK009-SRYKA                      PIC X(02).
           03  WRK-INNAI-MES                           PIC X(50).
           03  WRK-INGAI-MES                           PIC X(50).
           03  WRK-CHKKBN                              PIC X(01).
           03  WRK-CDKBN                               PIC X(01).
           03  WRK-NYUGAIBNDKBN                        PIC X(01).
           03  WRK-SYORIKBN                            PIC X(01).
           03  WRK-PTBYOMEI                            PIC X(200).
           03  WRK-PTBYOSRYYMD                         PIC X(08).
           03  WRK-LSRYCD                              PIC X(09).
           03  WRK-RSRYCD                              PIC X(09).
           03  WRK-TIYU-CHUSHI                         PIC 9(01).
           03  WRK-SAISHIN-ERRCD                       PIC X(06).
           03  WRK-HKNFLG                              PIC 9(01).
           03  WRK-SRTKBN                              PIC X(01).
           03  WRK-BYOEDYMD                            PIC X(08).
           03  WRK-ZAISKBKBN                           PIC X(01).
           03  WRK-CHKDAY-G.
               05  WRK-CHKDAY                          PIC 9(01)
                                                       OCCURS  31.
           03  WRK-DAY-G.
               05  WRK-DAY                             PIC 9(02).
           03  WRK-LASTYMD                             PIC X(08).
           03  WRK-REIGAI-CHKKBN                       PIC X(01).
           03  WRK-REIGAI-CHKNUM.
               05  WRK-REIGAI-CHKNUM9                  PIC 9(02).
           03  WRK-REIGAI-CD                           PIC X(160).
           03  WRK-REIGAI-TEKCD                        PIC X(160).
           03  WRK-REIGAI-COMPKBN                      PIC X(1).
           03  WRK-REIGAI-TANICD                       PIC 9(03).
           03  WRK-SURYOZ-G.
               05  WRK-SURYOZ                          PIC ZZZZ9.9(5).
               05  WRK-SURYOZ-R    REDEFINES           WRK-SURYOZ.
                   07  WRK-SURYOZ1                     PIC X(05).
                   07  WRK-SURYOZDOT                   PIC X(01).
                   07  WRK-SURYOZ2                     PIC X(05).
           03  WRK-SRY-NAME                            PIC X(50).
           03  WRK-ERRCOM                              PIC X(80).
           03  WRK-MAXLEN                              PIC 9(08).
           03  WRK-COMLEN                              PIC 9(08).
           03  WRK-TNSLEN                              PIC 9(08).
           03  WRK-GETUMATU                            PIC X(08).
           03  WRK-AGE70.
               05  WRK-AGE70-YY                        PIC 9(04).
               05  WRK-AGE70-MM                        PIC 9(02).
               05  WRK-AGE70-DD                        PIC 9(02).
           03  WRK-INGAITEN                            PIC 9(07).
           03  WRK-058-G.
               05  WRK-058-SRYCD                       PIC X(09).
               05  WRK-058-TANICD                      PIC X(03).
           03  WRK-TEKBYOCHKKBN                        PIC X(01).
           03  WRK-ERR-KANA                            PIC X(100).
           03  WRK-JNISSU-DAY-G.
               05  WRK-JNISSU-DAY                      PIC 9(01)
                                                       OCCURS  31.
           03  WRK-ERR-ITEM-G.
               05  WRK-ERR-ITEM                        PIC X(50)
                                                       OCCURS  5.
           03  WRK-KAISU                               PIC 9(05).
           03  WRK-KAISU-X                             PIC X(05).
           03  WRK-TEKYMD.
               05  WRK-TEKYM                           PIC X(06).
               05  WRK-TEKDD                           PIC 9(02).
           03  WRK-TEMP-KEY                            PIC X(16).
      *
       01  TERRINF-AREA.
           03  TERRINF-CHKKBN                          PIC X(01).
           03  TERRINF-SRYCD                           PIC X(09).
           03  TERRINF-SRYKA                           PIC X(02).
           03  TERRINF-BYOMEI                          PIC X(160).
           03  TERRINF-TEKCD                           PIC X(09).
           03  TERRINF-TEKBYOMEI                       PIC X(160).
           03  TERRINF-TEKCOMPKBN                      PIC X(1).
           03  TERRINF-TANICD                          PIC 9(03).
           03  TERRINF-TANINAME                        PIC X(12).
           03  TERRINF-SHONUM                          PIC 9(03).
           03  TERRINF-SRYSTYMD                        PIC X(08).
           03  TERRINF-SRYEDYMD                        PIC X(08).
           03  TERRINF-LIMIT                           PIC 9(05).
           03  TERRINF-TSUKISU                         PIC 9(02).
           03  TERRINF-DAY-G.
               05  TERRINF-DAY                         PIC 9(01)
                                                       OCCURS  31.
           03  TERRINF-LASTYMD                         PIC X(08).
      *
       01  TKANRIRYO-AREA.
           03  TKANRIRYO-OCC                           OCCURS  9.
               05  TKANRIRYO-NYUGAIKBN                 PIC X(01).
               05  TKANRIRYO-FSANTEIYMD                PIC X(08).
               05  TKANRIRYO-DAY-G.
                   07  TKANRIRYO-DAY                   PIC 9(01)
                                                       OCCURS  31.
      *
       01  TPSET-AREA.
           03  TPSET-MAX                   PIC 9(05).
           03  TPSET-DAY-G.
               05  TPSET-DAY-OCC           OCCURS  31.
                   07  TPSET-DAY           PIC 9(01).
           03  TPSET-FLG-G.
               05  TPSET-FLG-OCC           OCCURS  31.
                   07  TPSET-FLG           PIC 9(01)
                                           OCCURS  100.
      *
       01  ERRKBN-AREA.
           03  ERRKBN-990009                           PIC X(01).
           03  ERRKBN-A10001                           PIC X(01).
      *
       01  KEY-AREA.
           03  KEY-HKNCOMBI                    PIC 9(04).
           03  KEY-SRYACT.
               05  KEY-SRY-PTID                PIC 9(10).
               05  KEY-SRY-ZAINUM              PIC 9(08).
               05  KEY-SRY-ACCTKBN             PIC X(01).
           03  KEY-SRYCD                       PIC X(09).
      *
      *    剤番号退避
       01  PTZAI-AREA.
        02 PTZAI-MAX                           PIC 9(05).
        02 PTZAI-OCC                           OCCURS  5000.
           03  PTZAI-ZAINUM                    PIC 9(08).
           03  PTZAI-ACCTKBN                   PIC X(01).
           03  PTZAI-NYUGAIKBN                 PIC X(01).
           03  PTZAI-HKNNUM                    PIC X(03).
           03  PTZAI-KOHNUM                    PIC X(03).
           03  PTZAI-DAY                       PIC 9(05)
                                               OCCURS  31.
           03  PTZAI-GROUP-OCC               OCCURS  100.
               05  PTZAI-GROUP.
                   07  PTZAI-SRYKA             PIC X(02).
                   07  PTZAI-DENPNUM           PIC 9(07).
               05  PTZAI-GROUP-KAISU         PIC 9(09).
               05  PTZAI-GROUP-DAY           PIC 9(02).
      *
      *    データチェック診療会計（患者別）
       01  PTACCT-AREA.
        02 PTACCT-MAX                          PIC 9(05).
        02 PTACCT-OCC                          OCCURS  5000.
           COPY    "CPDCACCT.INC"              REPLACING  //DCACCT-//
                                               BY         //PTACCT-//.
      *
      *
      *    診療行為（患者別）
       01  PTSRY-AREA.
        02 PTSRY-MAX                           PIC 9(05).
        02 PTSRY-OCC                           OCCURS  5000.
           COPY    "CPDCSRY.INC"               REPLACING  //DCSRY-//
                                               BY         //PTSRY-//.
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-SETCD-MAX                     PIC 9(05)
                                                   VALUE  100.
           03  CONST-TTNSP2-MAX                    PIC 9(05)
                                                   VALUE  1000.
           03  CONST-INPUT-MAX                     PIC 9(05)
                                                   VALUE   400.
           03  CONST-THKNID-MAX                    PIC 9(05)
                                                   VALUE   20.
           03  CONST-TKOHID-MAX                    PIC 9(05)
                                                   VALUE   20.
           03  CONST-THKNCOMBI-MAX                 PIC 9(05)
                                                   VALUE   40.
           03  CONST-TKHNRYO-HKNCOMBI-MAX          PIC 9(05)
                                                   VALUE   20.
           03  CONST-TSRYKA-MAX                    PIC 9(05)
                                                   VALUE   30.
           03  CONST-TSRYKA-SRYCD-MAX              PIC 9(05)
                                                   VALUE   1000.
           03  CONST-TSRYKA-PSRYCD-MAX              PIC 9(05)
                                                   VALUE   1000.
           03  CONST-TSRYCD-MAX                    PIC 9(05)
                                                   VALUE   2000.
           03  CONST-GROUP-MAX                   PIC 9(05)
                                                   VALUE   100.
           03  CONST-TJNISSU-MAX                   PIC 9(05)
                                                   VALUE   20.
           03  CONST-TKAISU-MAX                    PIC 9(05)
                                                   VALUE   20.
           03  CONST-TKAISU-SRYCD-MAX              PIC 9(05)
                                                   VALUE   2000.
           03  CONST-THEIYOU-MAX                   PIC 9(05)
                                                   VALUE   2000.
           03  CONST-TBYOMEICD-MAX                 PIC 9(05)
                                                   VALUE   100.
           03  CONST-TNYUKSN-MAX                   PIC 9(05)
                                                   VALUE   20.
           03  CONST-TCOMERR-ZAI-MAX               PIC 9(05)
                                                   VALUE   30.
           03  CONST-TCHK-MAX                      PIC 9(05)
                                                   VALUE   2000.
           03  CONST-TCHK-STK-CD-MAX               PIC 9(05)
                                                   VALUE   400.
           03  CONST-THKN-MAX                      PIC 9(05)
                                                   VALUE   100.
           03  CONST-PTACCT-MAX                    PIC 9(05)
                                                   VALUE   5000.
           03  CONST-PTSRY-MAX                     PIC 9(05)
                                                   VALUE   5000.
           03  CONST-PTZAI-MAX                     PIC 9(05)
                                                   VALUE   5000.
           03  CONST-TINGAI-MAX                      PIC 9(05)
                                                   VALUE   20.
           03  CONST-TKZI-MAX                      PIC 9(05)
                                                   VALUE   30.
           03  CONST-TKZI-MICD-MAX                 PIC 9(05)
                                                   VALUE   30.
           03  CONST-TRSIRECEDEN-MAX               PIC 9(05)
                                                   VALUE   20.
           03  CONST-TKAIGO-MAX                    PIC 9(05)
                                                   VALUE   50.
           03  CONST-CHKPTN                        PIC X(05)
                                                   VALUE   "1 111".
           03  CONST-NBHOUKTCD                     PIC X(09)
                                                   VALUE   "099600801".
           03  CONST-GENZAN-MAX                    PIC 9(03)
                                                   VALUE   3.
           03  CONST-SYAHO                         PIC X(01)
                                                   VALUE   "1".
           03  CONST-KOKUHO                        PIC X(01)
                                                   VALUE   "2".
           03  CONST-ROSAI                         PIC X(01)
                                                   VALUE   "3".
           03  CONST-JIBAI                         PIC X(01)
                                                   VALUE   "4".
           03  CONST-JIHI                          PIC X(01)
                                                   VALUE   "5".
           03  CONST-KOUGAI                        PIC X(01)
                                                   VALUE   "6".
           03  CONST-KOUKI                         PIC X(01)
                                                   VALUE   "7".
           03  CONST-CHIKEN                        PIC X(01)
                                                   VALUE   "8".
           03  CONST-DAISAN                        PIC X(01)
                                                   VALUE   "9".
           03  CONST-HOUKATU                       PIC X(01)
                                                   VALUE   "H".
           03  CONST-MKCD-MAX                      PIC 9(05)
                                                   VALUE 21.
      *
           03  CONST-TSANTEICD-G.
               05  CONST-TSANTEICD-MAX         PIC 9(05)
                                               VALUE   38.
               05  CONST-TSANTEICD-VAL.
                   07                          PIC X(11)
                                               VALUE   "113006510MM".
                   07                          PIC X(11)
                                               VALUE   "180029910RS".
                   07                          PIC X(11)
                                               VALUE   "180030010RS".
                   07                          PIC X(11)
                                               VALUE   "180030110RN".
                   07                          PIC X(11)
                                               VALUE   "180030210RN".
                   07                          PIC X(11)
                                               VALUE   "180030310RU".
                   07                          PIC X(11)
                                               VALUE   "180030410RU".
                   07                          PIC X(11)
                                               VALUE   "180030510RK".
                   07                          PIC X(11)
                                               VALUE   "180030610RK".
      *
                   07                          PIC X(11)
                                               VALUE   "140029210CR".
                   07                          PIC X(11)
                                               VALUE   "140002210CM".
                   07                          PIC X(11)
                                               VALUE   "140002310CM".
                   07                          PIC X(11)
                                               VALUE   "140002450CM".
                   07                          PIC X(11)
                                               VALUE   "140029610CM".
                   07                          PIC X(11)
                                               VALUE   "140030350CM".
                   07                          PIC X(11)
                                               VALUE   "140040310CM".
                   07                          PIC X(11)
                                               VALUE   "140048010CM".
                   07                          PIC X(11)
                                               VALUE   "140048250CM".
                   07                          PIC X(11)
                                               VALUE   "140048350CM".
                   07                          PIC X(11)
                                               VALUE   "140048450CM".
                   07                          PIC X(11)
                                               VALUE   "140048550CM".
                   07                          PIC X(11)
                                               VALUE   "180027410CS".
                   07                          PIC X(11)
                                               VALUE   "180027510CS".
                   07                          PIC X(11)
                                               VALUE   "180029110CS".
                   07                          PIC X(11)
                                               VALUE   "180029210CS".
                   07                          PIC X(11)
                                               VALUE   "180027610CN".
                   07                          PIC X(11)
                                               VALUE   "180027710CN".
                   07                          PIC X(11)
                                               VALUE   "180029310CN".
                   07                          PIC X(11)
                                               VALUE   "180029410CN".
                   07                          PIC X(11)
                                               VALUE   "180030810CN".
                   07                          PIC X(11)
                                               VALUE   "180027810CU".
                   07                          PIC X(11)
                                               VALUE   "180027910CU".
                   07                          PIC X(11)
                                               VALUE   "180029510CU".
                   07                          PIC X(11)
                                               VALUE   "180029610CU".
                   07                          PIC X(11)
                                               VALUE   "180028010CK".
                   07                          PIC X(11)
                                               VALUE   "180028110CK".
                   07                          PIC X(11)
                                               VALUE   "180029710CK".
                   07                          PIC X(11)
                                               VALUE   "180029810CK".
               05  CONST-TSANTEICD-R   REDEFINES   CONST-TSANTEICD-VAL.
                   07  CONST-TSANTEICD-OCC     OCCURS  38.
                       09   CONST-TSANTEICD-SRYCD
                                               PIC X(09).
                       09   CONST-TSANTEICD-TYPE
                                               PIC X(02).
           03  CONST-NUM-G.
               05  CONST-NUM-VAL               PIC X(10)
                                               VALUE   "0123456789".
               05  CONST-NUM-R         REDEFINES   CONST-NUM-VAL.
                   07  CONST-NUM               PIC X(01)   OCCURS    10.
               05  CONST-ZENNUM-VAL            PIC X(20)
                                       VALUE   "０１２３４５６７８９".
               05  CONST-ZENNUM-R      REDEFINES   CONST-ZENNUM-VAL.
                   07  CONST-ZENNUM            PIC X(02)   OCCURS    10.
           03  CONST-H039                      PIC X(03)   VALUE "039".
           03  CONST-H009                      PIC X(03)   VALUE "009".
           03  CONST-H2004                 PIC X(06)   VALUE "200804".
           03  CONST-H2603                 PIC X(06)   VALUE "201403".
           03  CONST-H2507                 PIC X(06)   VALUE "201307".
           03  CONST-H2804                 PIC X(06)   VALUE "201604".
           03  CONST-H2904                 PIC X(06)   VALUE "201704".
           03  CONST-R0204                 PIC X(06)   VALUE "202004".
           03  CONST-TEMP                      PIC X(04)   VALUE "TEMP".
           03  CONST-RSI-KATAKANA-G.
               05  CONST-RSI-KATAKANA-VAL.
                   07  FILLER                      PIC X(32)   VALUE
                       "ァアィイゥウェエォオカガキギクグ".
                   07  FILLER                      PIC X(32)   VALUE
                       "ケゲコゴサザシジスズセゼソゾタダ".
                   07  FILLER                      PIC X(32)   VALUE
                       "チヂッツヅテデトドナニヌネノハバ".
                   07  FILLER                      PIC X(32)   VALUE
                       "パヒビピフブプヘベペホボポマミ　".
                   07  FILLER                      PIC X(32)   VALUE
                       "ムメモャヤュユョヨラリルレロヮワ".
                   07  FILLER                      PIC X(16)   VALUE
                       "ヰヱヲンヴヵヶー".
               05  CONST-RSI-KATAKANA-R REDEFINES
                                        CONST-RSI-KATAKANA-VAL.
                   07  CONST-RSI-KATAKANA      PIC X(02)   OCCURS 88.
               05  CONST-RSI-KATAKANA-MAX      PIC 9(05)   VALUE  88.
      *
      *    エラーメッセージ
           COPY    "CPDTCHKERRMES.INC".
      *
           COPY    "COMMON-SPA".
      *
           COPY    "CPTTNS.INC".
      *
           COPY    "CPKGNMCHK.INC".
      *
       01  ERRPTINF-REC.
           COPY    "CPPTINF.INC"           REPLACING  //PTINF-//
                                           BY         //ERRPTINF-//.
       01  KEYCHKSND-REC.
           COPY    "CPCHKSND.INC"  REPLACING  //CHKSND-//
                                   BY         //KEYCHKSND-//.
      *
           COPY    "CPCHKREIGAI.INC"  REPLACING  //CHKREIGAI-//
                                      BY         //KEYREIGAI-//.
      *
       01  TCHKSND-REC.
           COPY    "CPCHKSND.INC"  REPLACING  //CHKSND-//
                                   BY         //TCHKSND-//.
      *
       01  TTNSP2-AREA.
         02    TTNSP2-MAX                  PIC     9(05).
         02    TTNSP2-IDX                  PIC     9(05).
         02    TTNSP2-FLG                  PIC     9(01).
         02    TTNSP2-OCC                  OCCURS  1000.
           COPY    "CPTENSUPLUS2.INC"      REPLACING  //TNSP2-//
                                   BY                 //TTNSP2-//.
      *
      *    保険番号
       01  THKN-AREA.
        02 THKN-SRH-HKNNUM         PIC X(03).
        02 THKN-SRH-PAYKBN         PIC X(02).
        02 THKN-IDX                PIC 9(05).
        02 THKN-HIT                PIC 9(01).
        02 THKN-MAX                PIC 9(05).
        02 THKN-OCC                OCCURS 100.
           COPY    "CPHKNNUM.INC"  REPLACING  //HKN-//
                                   BY         //THKN-//.
      *
      *    データチェック中間ファイルワーク
           COPY    "CPDTCHK001.INC"
                                   REPLACING  //DTCHK001//
                                   BY         //WKDTCHK003//.
       01  TPTRSIINF-REC.
           COPY    "CPPTRSIINF.INC"
                                   REPLACING  //PTRSI//
                                   BY         //TPTRSI//.
      *
           COPY    "CPDTCHK013.INC"
                                   REPLACING  //DTCHK013//
                                   BY         //WKDTCHK013//.
      *
       01  TPTBYOEX-AREA.
           03 TPTBYOEX-HAISHIKBN       PIC X(01).
           03 TPTBYOEX-HKNTGKBN        PIC X(01).
           03 TPTBYOEX-SHUSHOKUGO-CNT  PIC 9(05).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    点数付加２
       01  TNSP2-REC.
           COPY    "CPTENSUPLUS2.INC".
      *
      *    病名
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    労働基準監督署コード
       01  LABOR-SIO-REC.
           COPY    "CPLABOR-SIO.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    チェク情報
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    一時チェックマスタ２
       01  TMPSND-REC.
           COPY    "CPCHKSND.INC"  REPLACING  //CHKSND//
                                   BY         //TMPSND//.
      *
      *    チェックマスタ２
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *
      *    チェックマスタ３
       01  CHKTRD-REC.
           COPY    "CPCHKTRD.INC".
      *
       01  MATERIAL-K-REC.
           COPY    "CPMATERIAL-K.INC".
      *
      *    チェックマスタ４
           COPY    "CPCHK004.INC".
           COPY    "CPCK1102.INC".
           COPY    "CPCK1103.INC".
      *
      *    チェックマスタ５
       01  CHK005-REC.
           COPY    "CPCHK005.INC".
      *
      *    チェック例外マスタ
           COPY    "CPCHKREIGAI.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関基本情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    データチェック機能情報
           COPY    "CPSK1040.INC".
      *
      *    データチェック機能情報２
           COPY    "CPSK1101.INC".
      *
      *    レセプト編集区分
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC".
      *
      *    労災自賠医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    入院基本情報
      *    COPY    "CPSK5000.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    相互作用情報
       01  INTERACT-REC.
           COPY    "CPINTERACT.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入力コード
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    入力セット
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    診療コード置換
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    適応病名マスタ
       01  TEKIOUBYOMEI-REC.
           COPY    "CPTEKIOUBYOMEI.INC".
      *
      *    医薬品傷病名マスタ
       01  TEKIOU-DISEASE-REC.
           COPY    "CPTEKIOU-DISEASE.INC".
      *
      *    患者介護認定者情報
       01  PTCRNIN-REC.
           COPY    "CPPTCARE-NINTEI.INC".
      *
      *    レセプト記載マスタ
       01  RKISAI-REC.
           COPY    "CPRECEKISAI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    病名編集サブ
           COPY    "CPORCSBYOMEI.INC".
      *
      *    チェックマスタ病名編集サブ
           COPY    "CPORCSS012.INC".
      *
      *    一時チェックマスタ編集サブ
           COPY    "CPORCSS013.INC".
      *
      *    入院負担金計算サブ
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUDAY-REC-T.
         02 LNK-SYUDAY-REC     OCCURS 10.
           COPY  "CPSYUDAY.INC"  REPLACING  //SDAY-// BY //LNK-SDAY-//.
      *
           COPY    "CPORCS02MSG.INC".
      *
      *    文字列操作サブ
           COPY    "CPORCSSTRING.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
           COPY    "CPORCXKANACONV.INC".
      *    半角チェック
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    適応病名チェックサブ
           COPY    "CPORCSDTCHK01.INC".
      *
      *    記号番号チェックサブ
           COPY    "CPORCSDTCHK04.INC".
      *
      *    病名取得サブ（診療行為入力漏れ）
           COPY    "CPORCSDTCHK06.INC".
      *
      *    病名編集サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    保険組合せ取得サブ
           COPY    "CPORCSDTCHK02.INC".
           01  LNK-HKNCOMBI-AREA.
             02  LNK-HKNCOMBI-OCC      OCCURS  1000.
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //LNK-COMB-//.
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
       01  DTCHK-RC        PIC S9(09).
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *
      *    エラーファイルファイル名称領域 
           COPY    "CPRECEERR.INC"   REPLACING  //RECEERR//
                                   BY         //LNKDTCHKERRPARA//.
      *
      *    データチェック一括処理パラメータ
           COPY    "CPORCBSD01PARA.INC".
      ****************************************************************
       PROCEDURE                       DIVISION
               USING
                                               DTCHK-RC
                                               WRK-PARA
                                               LNKDTCHKERRPARA
                                               ORCBSD01PARA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           MOVE    FLG-ERR             TO  DTCHK-RC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  STS-AREA
                                       IDX-AREA
                                       CNT-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       SPA-AREA
                                       THKN-AREA
                                       WKDTCHK003-REC
      *
           MOVE    LNKDTCHKERRPARA     TO  DTCHKERRPARA
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           IF      LNK-PRTKANRI-TBL-KEY  NOT =  "RECEPTX"
               PERFORM 900-DBSTART-SEC
      *
      *    ステップ管理開始処理
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    "ORCDTCHK002"       TO  JOB-PGID
               MOVE    "データチェック"    TO  JOB-SHELLMSG
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
      *
               PERFORM 1001-CHKSND-CD-EDIT-SEC
      *
           END-IF
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           MOVE    SPA-HOSPNUM     TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC      TO  MCPDATA-REC
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "tmpkey"        TO  MCP-PATHNAME
           PERFORM 910-DBCREATE-SEC
      *
           MOVE    CHKSND-REC      TO  MCPDATA-REC
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "tmpkey"        TO  MCP-PATHNAME
           PERFORM 910-DBCREATEINDEX-SEC
      *
           MOVE    ZERO                TO  FLG-END
                                           FLG-ERR
      *
      *    データチェック中間ファイル名
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHKACCTPARA-HOSPNUM
           MOVE    "DCACCT00"          TO  DTCHKACCTPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHKACCTPARA-TERMID
           MOVE    DTCHKACCTPARA-BASENAME  TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA                TO  DTCHKACCTPARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHKSRYPARA-HOSPNUM
           MOVE    "DCSRY000"          TO  DTCHKSRYPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHKSRYPARA-TERMID
           MOVE    DTCHKSRYPARA-BASENAME   TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHKSRYPARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHKSANTEIPARA-HOSPNUM
           MOVE    "DCSANTEI"          TO  DTCHKSANTEIPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHKSANTEIPARA-TERMID
           MOVE    DTCHKSANTEIPARA-BASENAME    TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHKSANTEIPARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK003PARA-HOSPNUM
           MOVE    "DTCHK003"          TO  DTCHK003PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK003PARA-TERMID
           MOVE    DTCHK003PARA-BASENAME   TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK003PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK004PARA-HOSPNUM
           MOVE    "DTCHK004"          TO  DTCHK004PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK004PARA-TERMID
           MOVE    DTCHK004PARA-BASENAME   TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK004PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK008PARA-HOSPNUM
           MOVE    "DTCHK008"          TO  DTCHK008PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK008PARA-TERMID
           MOVE    DTCHK008PARA-BASENAME   TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK008PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK009PARA-HOSPNUM
           MOVE    "DTCHK009"          TO  DTCHK009PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK009PARA-TERMID
           MOVE    DTCHK009PARA-BASENAME   TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK009PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK010PARA-HOSPNUM
           MOVE    "DTCHK010"          TO  DTCHK010PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK010PARA-TERMID
           MOVE    DTCHK010PARA-BASENAME   TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK010PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK011PARA-HOSPNUM
           MOVE    "DTCHK011"          TO  DTCHK011PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK011PARA-TERMID
           MOVE    DTCHK011PARA-BASENAME   TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK011PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK012PARA-HOSPNUM
           MOVE    "DTCHK012"          TO  DTCHK012PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK012PARA-TERMID
           MOVE    DTCHK012PARA-BASENAME   TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK012PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK013PARA-HOSPNUM
           MOVE    "DTCHK013"          TO  DTCHK013PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK013PARA-TERMID
           MOVE    DTCHK013PARA-BASENAME   TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK013PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK018PARA-HOSPNUM
           MOVE    "DTCHK018"          TO  DTCHK018PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK018PARA-TERMID
           MOVE    DTCHK018PARA-BASENAME   TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK018PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK022PARA-HOSPNUM
           MOVE    "DTCHK022"          TO  DTCHK022PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK022PARA-TERMID
           MOVE    DTCHK022PARA-BASENAME   TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK022PARA-FULLNAME
      *
           OPEN    INPUT               DCACCT-FILE
                                       DCSRY-FILE
                                       DTCHK003-FILE
      *
           OPEN    OUTPUT              DTCHK004-FILE
      *
           OPEN    OUTPUT              DTCHK008-FILE
           CLOSE   DTCHK008-FILE
           OPEN    I-O                 DTCHK008-FILE
      *
           OPEN    OUTPUT              DTCHK010-FILE
           CLOSE   DTCHK010-FILE
           OPEN    I-O                 DTCHK010-FILE
      *
           OPEN    OUTPUT              DTCHK011-FILE
      *
           OPEN    OUTPUT              DTCHK012-FILE
           CLOSE   DTCHK012-FILE
           OPEN    I-O                 DTCHK012-FILE
      *
           OPEN    INPUT               DTCHK013-FILE
      *
           OPEN    INPUT               DTCHK018-FILE
      *
           OPEN    INPUT               DTCHK022-FILE
      *
      *    初期値設定処理
           PERFORM 1001-INIT-VAL-SET-SEC
      *
      *    チェックマスタチェック処理
           PERFORM 1001-CHKMST-CHK-SEC
      *
      *    保険の種類・公費の種類編集処理
           PERFORM 1001-HKNKOH-HEN-SEC
      *
      *    医療機関基本情報
           PERFORM 1001-SYS1001-GET-SEC
      *
      *    診療科編集処理
           PERFORM 1001-SRYKA-HEN-SEC
      *
      *    負担者番号受給者番号抜けチェック対象取得処理
           PERFORM 1001-SYS1040-GET-SEC
      *
      *    データチェック２機能情報
           PERFORM 1001-SYS1101-GET-SEC
      *
      *    レセプト編集区分取得処理
           PERFORM 1001-SYS2005-GET-SEC
      *
      *    労災自賠医療機関情報取得処理
           PERFORM 1001-SYS4001-GET-SEC
      *
      *    記号抜けチェック対象取得処理
           PERFORM 1001-CK4-1102-GET-SEC
      *
      *    チェック可否設定取得処理
           PERFORM 1001-CK4-1103-GET-SEC
      *
           IF    ( SYS-1101-REIGAICHKKBN  =   "1" )
               PERFORM 900-CHKREIGAI-KEY2-SEL-SEC
               PERFORM UNTIL ( FLG-CHKREIGAI   NOT =   ZERO )
                   MOVE    CHKREIGAI-REC   TO  DTCHK011-REC
                   WRITE   DTCHK011-REC
                   PERFORM 900-CHKREIGAI-KEY2-FET-SEC
               END-PERFORM
               MOVE    "tbl_chkreigai"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
           CLOSE   DTCHK011-FILE
           OPEN    INPUT                 DTCHK011-FILE
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期値設定処理
      *****************************************************************
       1001-INIT-VAL-SET-SEC           SECTION.
      *
           MOVE    ORCBSD01-SRYYM      TO  WRK-KJNYMD
           MOVE    "01"                TO  WRK-KJNYMD(7:2)
      *
           MOVE    ORCBSD01-SRYYM      TO  WRK-SYMD
           PERFORM 800-GETUMATU-SEC
           MOVE    WRK-SYMD            TO  WRK-GETUMATU
      *
           INITIALIZE                      TCHK-AREA
                                           TTNS-AREA
                                           TTNSP2-AREA
      *
           MOVE    CONST-NBHOUKTCD     TO  WRK-SRYCD
           MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           MOVE    TNS-TEN             TO  WRK-TNS-NBFUKATEN
      *
           .
       1001-INIT-VAL-SET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２適用コード編集処理
      *****************************************************************
       1001-CHKSND-CD-EDIT-SEC         SECTION.
      *
           PERFORM 900-DBCOMMIT-SEC
           PERFORM 900-DBSTART-SEC
      *
           PERFORM 900-CHKSND-KEY28-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
      *
               IF    ( CHKSND-BYOMEI   NOT =   SPACE )
                AND  ( CHKSND-CD           =   SPACE )
      *
                   INITIALIZE              SS012-AREA
                   MOVE    CHKSND-BYOMEI
                                       TO  SS012-I-BYOMEI
                   CALL    "ORCSS012"      USING
                                           SS012-AREA
                                           SPA-AREA
                   MOVE    SS012-BYOMEICD-G
                                       TO  CHKSND-CD
      *
                   MOVE    CHKSND-REC  TO  MCPDATA-REC
                   MOVE    "tbl_chksnd"    TO  MCP-TABLE
                   MOVE    "key28"         TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
               END-IF
      *
               PERFORM 900-CHKSND-KEY28-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key28"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM 900-DBCOMMIT-SEC
           PERFORM 900-DBSTART-SEC
      *
           .
      *
       1001-CHKSND-CD-EDIT-EXT.
           EXIT.
           .
      *****************************************************************
      *    チェックマスタチェック処理
      *    （チェック用データの存在チェック）
      *****************************************************************
       1001-CHKMST-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-YKZBYO
                                           FLG-SRYBYO
                                           FLG-YKZHEI
                                           FLG-SRYHEI
                                           FLG-YKZKNKBYO
                                           FLG-SRYSNT
                                           FLG-BYOYKZ
                                           FLG-BYOSRY
                                           FLG-SRYHEIA
                                           FLG-SRYHEIB
      *
      *    薬剤と病名
      *    MOVE    "1"                 TO  WRK-CHK-CHKKBN
      *    MOVE    "1"                 TO  WRK-CHK-CDKBN
      *    PERFORM 900-CHKSND-KEY5-SEL-SEC
      *    IF    ( FLG-CHKMST          =   ZERO )
      *        MOVE    1               TO  FLG-YKZBYO
      *    END-IF
      *
           MOVE    1                   TO  FLG-YKZBYO
      *
      *    診療行為と病名
      *    MOVE    "2"                 TO  WRK-CHK-CHKKBN
      *    MOVE    "1"                 TO  WRK-CHK-CDKBN
      *    PERFORM 900-CHKSND-KEY5-SEL-SEC
      *    IF    ( FLG-CHKMST          =   ZERO )
      *        MOVE    1               TO  FLG-SRYBYO
      *    END-IF
      *
           MOVE    1                   TO  FLG-SRYBYO
      *
      *    併用禁忌の薬剤
           MOVE    "4"                 TO  WRK-CHK-CHKKBN
           MOVE    "2"                 TO  WRK-CHK-CDKBN
           PERFORM 900-CHK-KEY5-SEL-SEC
           IF    ( FLG-CHKMST          =   ZERO )
               MOVE    1               TO  FLG-YKZHEI
           END-IF
      *
      *    MOVE    1                   TO  FLG-YKZHEI
      *
      *    併用算定の診療行為
      *    MOVE    "5"                 TO  WRK-CHK-CHKKBN
      *    MOVE    "2"                 TO  WRK-CHK-CDKBN
      *    PERFORM 900-CHK-KEY5-SEL-SEC
      *    IF    ( FLG-CHKMST          =   ZERO )
      *        MOVE    1               TO  FLG-SRYHEI
      *    END-IF
      *
           MOVE    1                   TO  FLG-SRYHEI
      *
      *    薬剤と投与禁忌病名
           MOVE    "6"                 TO  WRK-CHK-CHKKBN
           MOVE    "0"                 TO  WRK-CHK-CDKBN
           PERFORM 901-CHKSND-KEY31-SEL-SEC
           IF    ( FLG-CHKMST          =   ZERO )
               MOVE    1               TO  FLG-YKZKNKBYO
           END-IF
      *
      *    診療行為どうしの算定漏れ
      *    MOVE    "7"                 TO  WRK-CHK-CHKKBN
      *    MOVE    "2"                 TO  WRK-CHK-CDKBN
      *    PERFORM 900-CHKTRD-KEY5-SEL-SEC
      *    IF    ( FLG-CHKMST          =   ZERO )
      *        MOVE    1               TO  FLG-SRYSNT
      *    END-IF
      *
           MOVE    1                   TO  FLG-SRYSNT
      *
      *    病名と薬剤
           MOVE    "8"                 TO  WRK-CHK-CHKKBN
           MOVE    "2"                 TO  WRK-CHK-CDKBN
           PERFORM 900-CHK005-KEY5-SEL-SEC
           IF    ( FLG-CHKMST          =   ZERO )
               MOVE    1               TO  FLG-BYOYKZ
           END-IF
      *
      *    病名と診療行為
           MOVE    "9"                 TO  WRK-CHK-CHKKBN
           MOVE    "2"                 TO  WRK-CHK-CDKBN
           PERFORM 900-CHK005-KEY5-SEL-SEC
           IF    ( FLG-CHKMST          =   ZERO )
               MOVE    1               TO  FLG-BYOSRY
           END-IF
      *
      *    併用算定の診療行為（同一日）
      *    MOVE    "A"                 TO  WRK-CHK-CHKKBN
      *    MOVE    "2"                 TO  WRK-CHK-CDKBN
      *    PERFORM 900-CHK-KEY5-SEL-SEC
      *    IF    ( FLG-CHKMST          =   ZERO )
      *        MOVE    1               TO  FLG-SRYHEIA
      *    END-IF
      *
           MOVE    1                   TO  FLG-SRYHEIA
      *
      *    併用算定の診療行為（同一会計）
      *    MOVE    "B"                 TO  WRK-CHK-CHKKBN
      *    MOVE    "2"                 TO  WRK-CHK-CDKBN
      *    PERFORM 900-CHK-KEY5-SEL-SEC
      *    IF    ( FLG-CHKMST          =   ZERO )
      *        MOVE    1               TO  FLG-SRYHEIB
      *    END-IF
      *
           MOVE    1                   TO  FLG-SRYHEIB
      *
           IF    ( ORCBSD01-CHKKBN (9) NOT =   ZERO )
            AND  ( FLG-SRYBYO              =   1   )
               PERFORM 10011-CHKSND-SETCD-CHK-SEC
           END-IF
      *
           .
       1001-CHKMST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタチェック処理
      *    セットコード登録確認
      *****************************************************************
       10011-CHKSND-SETCD-CHK-SEC      SECTION.
      *
           INITIALIZE                      CHKSND-REC
           MOVE    "2"                 TO  CHKSND-CHKKBN
           MOVE    "0"                 TO  CHKSND-CDKBN
           MOVE    "P%"                TO  CHKSND-SRYCD
           MOVE    WRK-KJNYMD          TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM    TO  CHKSND-HOSPNUM
      *
           PERFORM 900-CHKSND-KEY31-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
      *
               INITIALIZE                  DTCHK010-REC
               MOVE    CHKSND-REC      TO  DTCHK010-DAT
               MOVE    "1"             TO  DTCHK010-CDKBN
      *
               MOVE    CHKSND-SRYCD    TO  WRK-SRYCD
      *
               PERFORM 900-INPUTSET-KEY7-SEL-SEC
      *
               MOVE    ORCBSD01-SRYYM  TO  WRK-YMD
               PERFORM UNTIL ( FLG-INPUTSET    NOT =   ZERO )
      *
      *            適応症の登録があるもののみ対象とする。
                   IF    ( CHKSND-RENNUM   >   ZERO )
      *
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
      *
                           MOVE    IDX-DAY TO  WRK-DD
                           IF    ( WRK-YMD    >= ISET-YUKOSTYMD )
                            AND  ( WRK-YMD    <= ISET-YUKOEDYMD )
                               IF    ( DTCHK010-SETCD-MAX (IDX-DAY)
                                                 >=    CONST-SETCD-MAX )
                                OR   ( ISET-INPUTCD (1:1)  =   "." )
                                   CONTINUE
                               ELSE
                                   MOVE    1   TO    DTCHK010-SETCD-GET
                                   COMPUTE DTCHK010-SETCD-MAX (IDX-DAY)
                                       =   DTCHK010-SETCD-MAX (IDX-DAY)
                                       +   1
                                   MOVE    DTCHK010-SETCD-MAX (IDX-DAY)
                                               TO  IDX-MAX
                                   MOVE    ISET-INPUTCD
                                               TO  DTCHK010-SETCD1
                                                  (IDX-DAY IDX-MAX)
      *
                                   MOVE    ISET-INPUTCD
                                               TO  WRK-SRYCD
                                   PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
                                   MOVE    CHG-RJNSRYCD
                                               TO  DTCHK010-SETCD2
                                                  (IDX-DAY IDX-MAX)
                               END-IF
                           END-IF
      *
                       END-PERFORM
      *
                   END-IF
      *
                   PERFORM 900-INPUTSET-KEY7-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_inputset"  TO  MCP-TABLE
               MOVE    "key7"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               IF    ( DTCHK010-SETCD-GET  =   1 )
                   WRITE   DTCHK010-REC
               END-IF
      *
               PERFORM 900-CHKSND-KEY31-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key31"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       10011-CHKSND-SETCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類・公費の種類編集処理
      *****************************************************************
       1001-HKNKOH-HEN-SEC         SECTION.
      *
           INITIALIZE                  SRH-HKNNUM-DAT
           INITIALIZE                  SRH-KOHNUM-DAT
      *
      *    保険番号検索
           MOVE    ZERO                TO  FLG-HKNNUM
           INITIALIZE                  HKNNUM-REC
           MOVE    WRK-PARA-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    WRK-KJNYMD          TO  HKN-TEKSTYMD
           MOVE    WRK-KJNYMD          TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             NOT =   ZERO )
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
               MOVE    "tbl_hknnum"    TO MCP-TABLE
               MOVE    "key3"          TO MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC             NOT =   ZERO )
                   MOVE    1           TO  FLG-HKNNUM
               ELSE
                   MOVE    MCPDATA-REC TO  HKNNUM-REC
               END-IF
      *
           END-PERFORM
      *
      *    保険番号クローズ
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-HKNKOH-CMB-EDIT-EXT.
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
      *    公費負担者番号・受給者番号抜けチェック対象取得処理
      *****************************************************************
       1001-SYS1001-GET-SEC            SECTION.
      *
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-KJNYMD          TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
           MOVE "tbl_syskanri"         TO  MCP-TABLE
           MOVE "key10"                TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-SYS1001-GET-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       1001-SRYKA-HEN-SEC              SECTION.
      *
           INITIALIZE              SRH-1005-DAT
      *
      *    システム管理マスタ検索処理
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-KJNYMD          TO  SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           END-IF
      *
           PERFORM VARYING IDX5    FROM    1   BY  1
               UNTIL     ( IDX5            >   100 )
                   OR    ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    IDX5            TO  SRH-1005-MAX
               MOVE    SYS-1005-KBNCD (1 : 2)
                                       TO  SRH-1005-SRYKAL (IDX5)
               MOVE    SYS-1005-SRYKANAME1
                                       TO  SRH-1005-SRYKAMEI1L (IDX5)
               MOVE    SYS-1005-SRYKANAME2
                                       TO  SRH-1005-SRYKAMEI2L (IDX5)
               MOVE    SYS-1005-SRYKANAME3
                                       TO  SRH-1005-SRYKAMEI3L (IDX5)
      *
      *        システム管理マスタ読込処理
               MOVE "tbl_syskanri"     TO  MCP-TABLE
               MOVE "key2"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC             NOT =   ZERO )
                   MOVE    1           TO  FLG-SYSKANRI
               ELSE
                   MOVE    MCPDATA-REC TO  SYS-1005-REC
               END-IF
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
      *    公費負担者番号・受給者番号抜けチェック対象取得処理
      *****************************************************************
       1001-SYS1040-GET-SEC            SECTION.
      *
      *
           INITIALIZE              SRH-1040-DAT
      *
      *    システム管理マスタ検索処理
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1040-REC
           MOVE    "1040"              TO  SYS-1040-KANRICD
           MOVE    WRK-KJNYMD          TO  SYS-1040-STYUKYMD
                                           SYS-1040-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1040-HOSPNUM
           MOVE    SYS-1040-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             NOT =   ZERO )
               MOVE    1               TO  FLG-SYSKANRI
           ELSE
               MOVE    MCPDATA-REC     TO  SYS-1040-REC
           END-IF
      *
           PERFORM VARYING IDX5    FROM    1   BY  1
               UNTIL     ( IDX5            >   100 )
                   OR    ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    IDX5            TO  SRH-1040-MAX
               MOVE    SYS-1040-KBNCD (1 : 3)
                                       TO  SRH-1040-KOHNUML (IDX5)
               MOVE    SYS-1040-FTNJANUMCHK
                                       TO  SRH-1040-FTNJANUMCHKL(IDX5)
               MOVE    SYS-1040-JKYSNUMCHK
                                       TO  SRH-1040-JKYSNUMCHKL (IDX5)
               MOVE    SYS-1040-FTNJHANKAKUCHK
                                   TO  SRH-1040-FTNJHANKAKUCHKL (IDX5)
               MOVE    SYS-1040-FTNJZENKAKUCHK
                                   TO  SRH-1040-FTNJZENKAKUCHKL (IDX5)
               MOVE    SYS-1040-FTNJNUMERICCHK
                                   TO  SRH-1040-FTNJNUMERICCHKL (IDX5)
               MOVE    SYS-1040-FTNJKETACHK
                                   TO  SRH-1040-FTNJKETACHKL (IDX5)
               MOVE    SYS-1040-FTNJKETAMIN
                                   TO  SRH-1040-FTNJKETAMINL (IDX5)
               MOVE    SYS-1040-FTNJKETAMAX
                                   TO  SRH-1040-FTNJKETAMAXL (IDX5)
               MOVE    SYS-1040-JKYSHANKAKUCHK
                                   TO  SRH-1040-JKYSHANKAKUCHKL (IDX5)
               MOVE    SYS-1040-JKYSZENKAKUCHK
                                   TO  SRH-1040-JKYSZENKAKUCHKL (IDX5)
               MOVE    SYS-1040-JKYSNUMERICCHK
                                   TO  SRH-1040-JKYSNUMERICCHKL (IDX5)
               MOVE    SYS-1040-JKYSKETACHK
                                   TO  SRH-1040-JKYSKETACHKL (IDX5)
               MOVE    SYS-1040-JKYSKETAMIN
                                   TO  SRH-1040-JKYSKETAMINL (IDX5)
               MOVE    SYS-1040-JKYSKETAMAX
                                   TO  SRH-1040-JKYSKETAMAXL (IDX5)
      *
      *        システム管理マスタ読込処理
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC             NOT =   ZERO )
                   MOVE    1           TO  FLG-SYSKANRI
               ELSE
                   MOVE    MCPDATA-REC TO  SYS-1040-REC
               END-IF
      *
           END-PERFORM
      *
      *    システム管理マスタクローズ処理
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-SYS1040-GET-EXT.
           EXIT.
      *****************************************************************
      *    データチェック２機能情報
      *****************************************************************
       1001-SYS1101-GET-SEC            SECTION.
      *
           INITIALIZE                      SYS-1101-REC
           MOVE    "1101"              TO  SYS-1101-KANRICD
           MOVE    "*"                 TO  SYS-1101-KBNCD
           MOVE    WRK-KJNYMD          TO  SYS-1101-STYUKYMD
                                           SYS-1101-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1101-HOSPNUM
           MOVE    SYS-1101-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1101-REC
           ELSE
               INITIALIZE                  SYS-1101-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( SYS-1101-SYA-INGAITEN   IS  NOT NUMERIC )
               MOVE    ZERO            TO  SYS-1101-SYA-INGAITEN
           END-IF
           IF    ( SYS-1101-KOK-INGAITEN   IS  NOT NUMERIC )
               MOVE    ZERO            TO  SYS-1101-KOK-INGAITEN
           END-IF
           IF    ( SYS-1101-KOU-INGAITEN   IS  NOT NUMERIC )
               MOVE    ZERO            TO  SYS-1101-KOU-INGAITEN
           END-IF
           IF    ( SYS-1101-RSI-INGAITEN   IS  NOT NUMERIC )
               MOVE    ZERO            TO  SYS-1101-RSI-INGAITEN
           END-IF
           IF    ( SYS-1101-JBI-INGAITEN   IS  NOT NUMERIC )
               MOVE    ZERO            TO  SYS-1101-JBI-INGAITEN
           END-IF
           IF    ( SYS-1101-JIH-INGAITEN   IS  NOT NUMERIC )
               MOVE    ZERO            TO  SYS-1101-JIH-INGAITEN
               END-IF
           IF    ( SYS-1101-KGI-INGAITEN   IS  NOT NUMERIC )
               MOVE    ZERO            TO  SYS-1101-KGI-INGAITEN
           END-IF
      *
           IF    ( SYS-1101-TEKBYOYKZKBN   =   SPACE )
               MOVE    ZERO            TO  SYS-1101-TEKBYOYKZKBN
           END-IF
           IF    ( SYS-1101-TEKBYOSRYKBN   =   SPACE )
               MOVE    ZERO            TO  SYS-1101-TEKBYOSRYKBN
           END-IF
           IF    ( SYS-1101-COMPBYONISSUKBN =  SPACE )
               MOVE    "1"             TO  SYS-1101-COMPBYONISSUKBN
           END-IF
           IF    ( SYS-1101-SRYSTTENKIKBN   =   SPACE )
               MOVE    "1"             TO  SYS-1101-SRYSTTENKIKBN
           END-IF
           IF    ( SYS-1101-YBSJTYKZKBN    =   SPACE )
               MOVE    "1"             TO  SYS-1101-YBSJTYKZKBN
           END-IF
           IF    ( SYS-1101-YBMSIYKZKBN    =   SPACE )
               MOVE    "1"             TO  SYS-1101-YBMSIYKZKBN
           END-IF
           IF    ( SYS-1101-YBSHOHOYKZKBN  =   SPACE )
               MOVE    "1"             TO  SYS-1101-YBSHOHOYKZKBN
           END-IF
           IF    ( SYS-1101-SRYKAISUMSTKBN =   SPACE )
               MOVE    "2"             TO  SYS-1101-SRYKAISUMSTKBN
           END-IF
           IF    ( SYS-1101-SRYKAISUWRNKBN =   SPACE )
               MOVE    "2"             TO  SYS-1101-SRYKAISUWRNKBN
           END-IF
           IF    ( SYS-1101-YKZHEIMSTKBN   =   SPACE )
               MOVE    "2"             TO  SYS-1101-YKZHEIMSTKBN
           END-IF
      *
           .
       1001-SYS1101-GET-EXT.
           EXIT.
      *****************************************************************
      *    レセプト編集区分取得処理
      *****************************************************************
       1001-SYS2005-GET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-RECEDEN-CHK
           MOVE    ZERO                TO  FLG-RSI-RECEDEN-CHK
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    WRK-KJNYMD          TO  SYS-2005-STYUKYMD
                                           SYS-2005-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-2005-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-2005-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-200501-REC
           MOVE    "2005"              TO  SYS-200501-KANRICD
           MOVE    "01"                TO  SYS-200501-KBNCD
           MOVE    WRK-KJNYMD          TO  SYS-200501-STYUKYMD
                                           SYS-200501-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-200501-HOSPNUM
           MOVE    SYS-200501-REC      TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-200501-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-200501-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
           IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
               EVALUATE   TRUE
               WHEN  ( SYS-2005-KKHRECEKBN =   "2" )
               WHEN  ( SYS-2005-KKHRECEKBN =   "3" )
               WHEN  ( SYS-2005-SYARECEKBN =   "2" )
               WHEN  ( SYS-2005-SYARECEKBN =   "3" )
               WHEN  ( SYS-200501-KOUIKIRECEKBN    =   "2" )
               WHEN  ( SYS-200501-KOUIKIRECEKBN    =   "3" )
                   MOVE    1           TO  FLG-RECEDEN-CHK
               END-EVALUATE
      *
               IF    ( SYS-200501-ROURECEKBN    =  "2" OR "3" )
                   MOVE    1           TO  FLG-RSI-RECEDEN-CHK
               END-IF
      *
           ELSE
               EVALUATE   TRUE
               WHEN  ( SYS-2005-KKHRECEKBN =   "1" )
               WHEN  ( SYS-2005-KKHRECEKBN =   "3" )
               WHEN  ( SYS-2005-SYARECEKBN =   "1" )
               WHEN  ( SYS-2005-SYARECEKBN =   "3" )
               WHEN  ( SYS-200501-KOUIKIRECEKBN    =   "1" )
               WHEN  ( SYS-200501-KOUIKIRECEKBN    =   "3" )
                   MOVE    1           TO  FLG-RECEDEN-CHK
               END-EVALUATE
      *
               IF    ( SYS-200501-ROURECEKBN    =  "1" OR "3" )
                   MOVE    1           TO  FLG-RSI-RECEDEN-CHK
               END-IF
           END-IF
      *
           .
       1001-SYS2005-GET-EXT.
           EXIT.
      *****************************************************************
      *    労災自賠医療機関情報取得処理
      *****************************************************************
       1001-SYS4001-GET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    WRK-KJNYMD          TO  SYS-4001-STYUKYMD
                                           SYS-4001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-4001-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-4001-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-SYS4001-GET-EXT.
           EXIT.
      *****************************************************************
      *    データチェック３機能情報
      *****************************************************************
       1001-CK4-1102-GET-SEC           SECTION.
      *
           INITIALIZE                  SRH-1102-DAT
      *
      *    システム管理マスタ検索処理
           MOVE    ZERO                TO  FLG-CHK004
           INITIALIZE                      CK4-1102-REC
           MOVE    "1102"              TO  CK4-1102-KANRICD
           MOVE    WRK-KJNYMD          TO  CK4-1102-STYUKYMD
                                           CK4-1102-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  CK4-1102-HOSPNUM
           MOVE    CK4-1102-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             NOT =   ZERO )
               MOVE    1               TO  FLG-CHK004
           ELSE
               MOVE    MCPDATA-REC     TO  CK4-1102-REC
           END-IF
      *
           PERFORM VARYING IDX5    FROM    1   BY  1
               UNTIL     ( IDX5            >   200 )
                   OR    ( FLG-CHK004    NOT =   ZERO )
      *
               MOVE    IDX5            TO  SRH-1102-MAX
               MOVE    CK4-1102-HKNJANUM
                                       TO  SRH-1102-HKNJANUML   (IDX5)
               MOVE    CK4-1102-HKNJANUMLEN
                                       TO  SRH-1102-HKNJANUMLENL(IDX5)
               MOVE    CK4-1102-JYKNKBN
                                       TO  SRH-1102-JYKNKBNL    (IDX5)
               MOVE    CK4-1102-HKNNUM
                                       TO  SRH-1102-HKNNUML     (IDX5)
               MOVE    CK4-1102-KIGOCHK
                                       TO  SRH-1102-KIGOCHKL    (IDX5)
      *
               MOVE    CK4-1102-KGCHKPTN
                                       TO SRH-1102-KGCHKPTNL    (IDX5)
               MOVE    CK4-1102-KGSUJI TO SRH-1102-KGSUJIL      (IDX5)
               MOVE    CK4-1102-KGKANJI
                                       TO SRH-1102-KGKANJIL     (IDX5)
               MOVE    CK4-1102-KGABT  TO SRH-1102-KGABTL       (IDX5)
               MOVE    CK4-1102-KGHYPN TO SRH-1102-KGHYPNL      (IDX5)
               MOVE    CK4-1102-KGKAKKO
                                       TO SRH-1102-KGKAKKOL     (IDX5)
               MOVE    CK4-1102-KGETC  TO SRH-1102-KGETCL       (IDX5)
               MOVE    CK4-1102-KGKANJINASHI
                                       TO SRH-1102-KGKANJINASHIL
                                                                (IDX5)
               MOVE    CK4-1102-NM1CHKPTN
                                       TO SRH-1102-NM1CHKPTNL   (IDX5)
               MOVE    CK4-1102-NM1SUJI
                                       TO SRH-1102-NM1SUJIL     (IDX5)
               MOVE    CK4-1102-NM1KANJI
                                       TO SRH-1102-NM1KANJIL    (IDX5)
               MOVE    CK4-1102-NM1ABT TO SRH-1102-NM1ABTL      (IDX5)
               MOVE    CK4-1102-NM1HYPN
                                       TO SRH-1102-NM1HYPNL     (IDX5)
               MOVE    CK4-1102-NM1KAKKO
                                       TO SRH-1102-NM1KAKKOL    (IDX5)
               MOVE    CK4-1102-NM1ETC TO SRH-1102-NM1ETCL      (IDX5)
               MOVE    CK4-1102-NMSUJINASHI
                                       TO SRH-1102-NMSUJINASHIL(IDX5)
               MOVE    CK4-1102-NM2CHKPTN
                                       TO SRH-1102-NM2CHKPTNL   (IDX5)
               MOVE    CK4-1102-NM2SUJI
                                       TO SRH-1102-NM2SUJIL     (IDX5)
               MOVE    CK4-1102-NM2KANJI
                                       TO SRH-1102-NM2KANJIL    (IDX5)
               MOVE    CK4-1102-NM2ABT TO SRH-1102-NM2ABTL      (IDX5)
               MOVE    CK4-1102-NM2HYPN
                                       TO SRH-1102-NM2HYPNL     (IDX5)
               MOVE    CK4-1102-NM2KAKKO 
                                       TO SRH-1102-NM2KAKKOL    (IDX5)
               MOVE    CK4-1102-NM2ETC TO SRH-1102-NM2ETCL      (IDX5)
               MOVE    CK4-1102-UPDDUMMY
                                       TO SRH-1102-UPDDUMMYL    (IDX5)
               MOVE    CK4-1102-NMKETAMIN
                                       TO SRH-1102-NMKETAMINL    (IDX5)
               MOVE    CK4-1102-NMKETAMAX
                                       TO SRH-1102-NMKETAMAXL    (IDX5)
               MOVE    CK4-1102-KGKETAMIN
                                       TO SRH-1102-KGKETAMINL    (IDX5)
               MOVE    CK4-1102-KGKETAMAX
                                       TO SRH-1102-KGKETAMAXL    (IDX5)
      *
               MOVE    "tbl_chk004"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC             NOT =   ZERO )
                   MOVE    1           TO  FLG-CHK004
               ELSE
                   MOVE    MCPDATA-REC TO  CK4-1102-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-CK4-1102-GET-EXT.
           EXIT.
      *****************************************************************
      *    データチェック３機能情報
      *****************************************************************
       1001-CK4-1103-GET-SEC           SECTION.
      *
           INITIALIZE                  ERRKBN-AREA
      *
           MOVE    "1"                 TO  ERRKBN-990009
      *
           MOVE    "2"                 TO  ERRKBN-A10001
      *
      *    システム管理マスタ検索処理
           MOVE    ZERO                TO  FLG-CHK004
           INITIALIZE                      CK4-1103-REC
           MOVE    "1103"              TO  CK4-1103-KANRICD
           MOVE    WRK-KJNYMD          TO  CK4-1103-STYUKYMD
                                           CK4-1103-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  CK4-1103-HOSPNUM
           MOVE    CK4-1103-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             NOT =   ZERO )
               MOVE    1               TO  FLG-CHK004
           ELSE
               MOVE    MCPDATA-REC     TO  CK4-1103-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-CHK004  NOT =   ZERO )
      *
               EVALUATE    CK4-1103-KBNCD
               WHEN    "99"
                   MOVE    CK4-1103-ERRKBN (9)
                                       TO  ERRKBN-990009
               WHEN    "A1"
                   MOVE    CK4-1103-ERRKBN (1)
                                       TO  ERRKBN-A10001
               END-EVALUATE
      *
               MOVE    "tbl_chk004"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF ( MCP-RC             NOT =   ZERO )
                   MOVE    1           TO  FLG-CHK004
               ELSE
                   MOVE    MCPDATA-REC TO  CK4-1103-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( ORCBSD01-SRYYM      >=  CONST-R0204 )
               MOVE    "2"             TO  ERRKBN-990009
           END-IF
      *
           .
       1001-CK4-1103-GET-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    データチェック処理
           PERFORM 2001-DATA-CHK-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    データチェック処理
      *****************************************************************
       2001-DATA-CHK-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK003
      *
      *    対象患者情報ファイル読込処理
           PERFORM 900-DTCHK003-READ-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK003    NOT =   ZERO )
                    OR   ( FLG-END         NOT =   ZERO )
      *
               MOVE    ZERO        TO  FLG-DATACHECK
               MOVE    ZERO        TO  FLG-CHKSKIP
      *
      *        患者情報取得処理
               PERFORM 900-PTINF-KEY-SEL-SEC
      *
               IF    ( ORCBSD01-IKTKBT     =   "1" )
                   IF    ( PTINF-TSTPTNUMKBN   NOT =   "1" )
                       MOVE    1   TO  FLG-DATACHECK
                   END-IF
               ELSE
                       MOVE    1   TO  FLG-DATACHECK
               END-IF
      *
               IF    ( FLG-DATACHECK   =   1 )
      *
      *            患者番号取得処理
                   PERFORM 900-PTNUM-KEY-SEL-SEC
      *
      *            チェック項目情報退避処理
                   PERFORM 20011-CHK-DATA-TAIHI-SEC
      *
      *            データチェック処理（チェックマスタ以外）
                   PERFORM 20011-DATA-CHK-SEC
      *
                   IF    ( FLG-CHKSKIP          =   ZERO )
      *
      *                データチェック処理（チェックマスタ）
                       PERFORM 20011-DATA-CHK-CHKMST-SEC
      *
      *                データチェック処理（チェックマスタ以外のマスタ）
                       PERFORM 20011-DATA-CHK-MST-OTHERS-SEC
      *
      *                禁忌チェック項目退避処理
                       PERFORM 20012-CHK-KINKI-TAIHI-SEC
      *
      *                禁忌データチェック処理
                       PERFORM 20012-KINKI-CHK-SEC
                   END-IF
      *
      *            処理経過編集処理
                   PERFORM 20011-SYORI-KEIKA-HEN-SEC
               END-IF
      *
      *        対象患者情報ファイル読込処理
               PERFORM 900-DTCHK003-READ-SEC
      *
           END-PERFORM
      *
           .
       2001-DATA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目情報退避処理
      *****************************************************************
       20011-CHK-DATA-TAIHI-SEC        SECTION.
      *
      *    退避エリア初期処理
           PERFORM 200111-TAIHI-AREA-INIT-SEC
      *
      *    患者保険組合せ情報取得
           PERFORM 200111-PTHKNCOMBI-GET-SEC
      *
      *    診療情報退避処理
           PERFORM 200111-SRYINF-TAIHI-SEC
      *
           .
       20011-CHK-DATA-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    退避エリア初期処理
      *****************************************************************
       200111-TAIHI-AREA-INIT-SEC      SECTION.
      *
           INITIALIZE                  THKNID-AREA
                                       TKOHID-AREA
                                       THKNCOMBI-AREA
                                       TNACCT-KKN-AREA
                                       TNYUINRYO-AREA
                                       TNACCT-GAIHAKU-AREA
                                       TSRYCD-AREA
                                       TJNISSU-AREA
                                       TNYUIN-CHOZAI-AREA
                                       TNYUIN-ACCT-AREA
                                       TNYUKSN-AREA
                                       TCOMERR-AREA
                                       TGENZAN-AREA
                                       TINGAI-AREA
                                       TKZI-AREA
                                       TRSIRECEDEN-AREA
                                       TKAIGO-AREA
                                       PTACCT-AREA
                                       PTSRY-AREA
                                       PTZAI-AREA
      *
           MOVE    ZERO            TO  FLG-INGAISYOHOU
      *
           PERFORM 200111-TSRYKA-AREA-INIT-SEC
      *
           PERFORM 200111-TKAISU-AREA-INIT-SEC
      *
      *
           .
       200111-TAIHI-AREA-INIT-EXT.
           EXIT.
      *****************************************************************
      *    診療科退避領域初期化処理
      *****************************************************************
       200111-TSRYKA-AREA-INIT-SEC     SECTION.
      *
           MOVE    ZERO            TO  TSRYKA-MAX
           INITIALIZE                  TSRYKA-ALL-G
           INITIALIZE                  TSRYKA-OCC (1)
      *
           PERFORM VARYING IDX0    FROM    2   BY  1
                   UNTIL ( IDX0    >   CONST-TSRYKA-MAX )
               MOVE    TSRYKA-OCC (1)  TO  TSRYKA-OCC (IDX0)
           END-PERFORM
      *
           .
       200111-TSRYKA-AREA-INIT-EXT.
           EXIT.
      *****************************************************************
      *    算定回数退避領域初期化処理
      *****************************************************************
       200111-TKAISU-AREA-INIT-SEC     SECTION.
      *
           MOVE    ZERO            TO  TKAISU-MAX
      *
           INITIALIZE                  TKAISU-OCC (1)
      *
           PERFORM VARYING IDX0    FROM    2   BY  1
                   UNTIL ( IDX0    >   CONST-TKAISU-MAX )
               MOVE    TKAISU-OCC (1)  TO  TKAISU-OCC (IDX0)
           END-PERFORM
      *
           .
       200111-TKAISU-AREA-INIT-EXT.
           EXIT.
      *****************************************************************
      *    患者保険組合せ情報取得
      *****************************************************************
       200111-PTHKNCOMBI-GET-SEC       SECTION.
      *
      *    保険組み合わせ
           INITIALIZE                  SDTCHK02AREA
                                       LNK-HKNCOMBI-AREA
      *
           MOVE    DTCHK003-HOSPNUM     TO  SDTCHK02-HOSPNUM
           MOVE    DTCHK003-PTID       TO  SDTCHK02-PTID
      *
           CALL    "ORCSDTCHK02"       USING   SDTCHK02AREA
                                               LNK-HKNCOMBI-AREA
                                               SPA-AREA
      *
           .
       200111-PTHKNCOMBI-GET-EXT.
           EXIT.
      *****************************************************************
      *    診療情報情報退避処理
      *****************************************************************
       200111-SRYINF-TAIHI-SEC         SECTION.
      *
           PERFORM 200111-DCSRY-TAIHI-SEC
      *
           PERFORM 200111-DCACCT-TAIHI-SEC
      *
           INITIALIZE                  KEY-AREA
      *
           PERFORM VARYING IDX-ZAI FROM    1   BY  1
                   UNTIL ( IDX-ZAI >   PTZAI-MAX )
      *
               IF    ( PTZAI-NYUGAIKBN (IDX-ZAI)
                                       =   ORCBSD01-NYUGAIKBN )
                   PERFORM VARYING IDX-ACCT    FROM    1   BY  1
                           UNTIL ( IDX-ACCT    >   PTACCT-MAX )
                       IF    ( PTZAI-ZAINUM (IDX-ZAI)
                                       =   PTACCT-ZAINUM (IDX-ACCT))
                        AND  ( PTZAI-ACCTKBN(IDX-ZAI)
                                       =   PTACCT-ACCTKBN(IDX-ACCT))
                           MOVE    PTACCT-OCC (IDX-ACCT)
                                   TO  DCACCT-REC
                           PERFORM 2001111-SRYINF-TAIHI-SEC
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           IF    ( ORCBSD01-NYUGAIKBN      =   "1" )
      *
               PERFORM 200111-TNYUINRYO-TAIHI-SEC
      *
               PERFORM 200111-NACCT-GAIHAKU-TAIHI-SEC
      *
           END-IF
      *
           .
       200111-SRYINF-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    対象保険チェック処理
      *****************************************************************
       2001111-TAISYO-HKN-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TAISYO-HKN
      *
           EVALUATE    TRUE
           WHEN      ( WRK-ZAIHKNNUM   =   "060" )
           WHEN      ( WRK-ZAIHKNNUM   =   "067" )
           WHEN      ( WRK-ZAIHKNNUM   =   "068" )
           WHEN      ( WRK-ZAIHKNNUM   =   "069" )
               IF    ( DTCHK003-HKNKBN =   CONST-KOKUHO )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "971" )
               IF    ( DTCHK003-HKNKBN =   CONST-ROSAI )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "970" )
               IF    ( DTCHK003-HKNKBN =   CONST-DAISAN )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "973" )
               IF    ( DTCHK003-HKNKBN =   CONST-JIBAI )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "975" )
               IF    ( DTCHK003-HKNKBN =   CONST-KOUGAI )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
           WHEN      ( WRK-ZAIHKNNUM   =   "040" )
           WHEN      ( WRK-ZAIHKNNUM   =   "039" )
               IF    ( DTCHK003-HKNKBN =   CONST-KOUKI )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
      *
           WHEN      ( WRK-ZAIHKNNUM (1:2) =   "98" )
               IF    ( DTCHK003-HKNKBN =   CONST-JIHI )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
      *
           WHEN      ( WRK-ZAIHKNNUM (1:2) =   "90" OR "91" )
               IF    ( DTCHK003-HKNKBN =   CONST-CHIKEN )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
      *
           WHEN      ( WRK-ZAIHKNNUM   =   "HHH" )
                   MOVE    1           TO  FLG-TAISYO-HKN
           WHEN      OTHER
               IF    ( DTCHK003-HKNKBN =   CONST-SYAHO )
                   MOVE    1           TO  FLG-TAISYO-HKN
               END-IF
           END-EVALUATE
      *
           .
       2001111-TAISYO-HKN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療情報情報退避処理
      *****************************************************************
       2001111-SRYINF-TAIHI-SEC        SECTION.
      *
           MOVE    PTZAI-HKNNUM (IDX-ZAI)
                                   TO   WRK-ZAIHKNNUM
           IF    ( PTZAI-KOHNUM (IDX-ZAI)  =   "970" )
               MOVE    PTZAI-KOHNUM (IDX-ZAI)
                                   TO   WRK-ZAIHKNNUM
           END-IF
           PERFORM 2001111-TAISYO-HKN-CHK-SEC
           IF    ( FLG-TAISYO-HKN      =   1 )
           AND   ( DCACCT-STS-NYUINDAY =   ZERO )
      *
               IF    ( DCACCT-HKNCOMBI     =   9999 )
                OR   ( DCACCT-JIHOKBN      =   "1" )
                   CONTINUE
               ELSE
                   PERFORM 20011111-SRYINF-TAIHI-SEC
               END-IF
      *
               PERFORM 20099999-SRYINF-TAIHI-SEC
      *
           END-IF
      *
           IF    ( DCACCT-STS-NYUINDAY     =   1 )
               PERFORM 200111-NACCT-KKN-TAIHI-SEC
           END-IF
      *
           .
       2001111-SRYINF-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    診療情報情報退避処理
      *****************************************************************
       20011111-SRYINF-TAIHI-SEC        SECTION.
      *
      *    保険組合せ退避ワーク編集処理
           PERFORM 200111-THKNCOMBI-HEN-SEC
      *
           IF    ( DCACCT-ACCTRENNUM   =   1 )
      *
               MOVE    ZERO        TO  IDX-TSRYKA
               MOVE    ZERO        TO  FLG-TSRYKA-OVER
               PERFORM 200111-TSRYKA-SRYKA-HEN-SEC
      *
      *        保険組合せ取得処理
               IF    ( KEY-HKNCOMBI    NOT =   DCACCT-HKNCOMBI )
      *
                   MOVE    DCACCT-HKNCOMBI TO  WRK-HKNCOMBI
                   PERFORM 220-HKNCOMBI-GET-SEC
                   MOVE    DCACCT-HKNCOMBI TO  KEY-HKNCOMBI
      *
      *            主保険チェック処理
                   PERFORM 200111-SYUHOKEN-CHK-SEC
      *
      *            保険ＩＤ退避ワーク編集チェック処理
                   PERFORM 200111-THKNID-HEN-CHK-SEC
      *
      *            公費ＩＤ退避ワーク編集チェック処理
                   PERFORM 200111-TKOHID-HEN-CHK-SEC
               END-IF
      *
      *        診療行為情報退避処理
               PERFORM 200111-SRYACT-TAIHI-SEC
      *
            END-IF
      *
            IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
      *
      *        入院料加算退避ワーク編集処理
               IF    ( DCACCT-ACCTKBN  =   "1" )
                AND  ( DCACCT-SRYKBN   =   "90"    OR  "92" )
                   PERFORM 200111-TNYUKSN-HEN-SEC
               END-IF
      *
      *        入院診療行為日編集処理（実日数チェック用）
               IF    ( DCACCT-ACCTKBN  =   "1" )
                   PERFORM 200111-TNYUIN-ACCT-HEN-SEC
               END-IF
      *
      *        入院調剤料算定日編集処理（実日数チェック用）
               IF    ( DCACCT-ACCTKBN  =   "1" )
                AND  ( DCACCT-SRYKBN   =   "24" OR "27" )
                   PERFORM 200111-TNYUIN-CHOZAI-HEN-SEC
               END-IF
      *
           END-IF
      *
           .
       20011111-SRYINF-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    診療情報情報退避処理
      *****************************************************************
       20099999-SRYINF-TAIHI-SEC        SECTION.
      *
           PERFORM VARYING IDX-SRY FROM    1   BY  1
                   UNTIL ( IDX-SRY     >   PTSRY-MAX )
      *
               IF    ( DCACCT-ZAINUM   =   PTSRY-ZAINUM (IDX-SRY) )
                AND  ( DCACCT-ACCTKBN  =   PTSRY-ACCTKBN(IDX-SRY) )
      *
                   MOVE    PTSRY-OCC (IDX-SRY)
                                       TO  DCSRY-REC
      *
      *            点数マスタ検索処理
                   MOVE    DCSRY-SRYCD     TO  WRK-SRYCD
                   MOVE    DCACCT-SRYYMD   TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
      *
                   IF    ( ORCBSD01-SRYYM     >=  CONST-H2904 )
                    AND  ( DCSRY-SRYCD (1:3)   =   "059" )
                    AND  ( TNS-TEN         NOT =   ZERO )
                       PERFORM 200111-TKZI-MICD-HEN-SEC
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       20099999-SRYINF-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    会計情報情報退避処理
      *****************************************************************
       200111-DCACCT-TAIHI-SEC         SECTION.
      *
           PERFORM 900-DCACCT-START-SEC
      *
           PERFORM UNTIL ( FLG-DCACCT NOT =   ZERO )
                    OR   ( PTACCT-MAX    >=   CONST-PTACCT-MAX )
      *
               COMPUTE PTACCT-MAX  =   PTACCT-MAX   +   1
      *
               MOVE    DCACCT-REC  TO  PTACCT-OCC (PTACCT-MAX)
      *
               PERFORM 200111-PTZAI-TAIHI-SEC
      *
               PERFORM 900-DCACCT-READ-SEC
      *
           END-PERFORM
      *
           .
       200111-DCACCT-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    剤番号退避処理
      *****************************************************************
       200111-PTZAI-TAIHI-SEC          SECTION.
      *
           PERFORM VARYING IDX-ZAI FROM    1   BY  1
                   UNTIL ( IDX-ZAI >   PTZAI-MAX )
                    OR  (( DCACCT-ZAINUM   =   PTZAI-ZAINUM (IDX-ZAI))
                     AND ( DCACCT-ACCTKBN  =   PTZAI-ACCTKBN(IDX-ZAI)))
      *
                   CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-ZAI >   PTZAI-MAX )
               IF    ( CONST-PTZAI-MAX >   PTZAI-MAX )
      *
                   COMPUTE PTZAI-MAX   =   PTZAI-MAX   +   1
      *
                   MOVE    DCACCT-ZAINUM   TO  PTZAI-ZAINUM (PTZAI-MAX)
                   MOVE    DCACCT-ACCTKBN  TO  PTZAI-ACCTKBN(PTZAI-MAX)
                   MOVE    DCACCT-NYUGAIKBN
                                           TO  PTZAI-NYUGAIKBN
                                                            (PTZAI-MAX)
                   IF    ( DCACCT-HKNCOMBI =   9999 )
                       MOVE    "HHH"       TO  PTZAI-HKNNUM (PTZAI-MAX)
                   ELSE
                       MOVE    DCACCT-HKNNUM
                                           TO  PTZAI-HKNNUM (PTZAI-MAX)
                   END-IF
                   MOVE    DCACCT-KOHNUM   TO  PTZAI-KOHNUM (PTZAI-MAX)
                   MOVE    DCACCT-DAYCNT   TO  PTZAI-DAY
                                                 (PTZAI-MAX DCACCT-DAY)
      *
                   MOVE    PTZAI-MAX       TO  IDX-ZAI
                   PERFORM 2001111-PTZAI-TAIHI-SEC
      *
               END-IF
           ELSE
               COMPUTE PTZAI-DAY (IDX-ZAI DCACCT-DAY)
                   =   PTZAI-DAY (IDX-ZAI DCACCT-DAY)
                   +   DCACCT-DAYCNT
               PERFORM 2001111-PTZAI-TAIHI-SEC
           END-IF
      *
           .
       200111-PTZAI-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    剤番号退避処理
      *****************************************************************
       2001111-PTZAI-TAIHI-SEC         SECTION.
      *
           PERFORM VARYING IDXN    FROM    1   BY  1
                   UNTIL ( IDXN    >   9 )
      *
               IF    ( DCACCT-DAYCNT2-IDX (IDXN)
                                   NOT =   ZERO )
      *
                   MOVE    DCACCT-DAYCNT2-IDX (IDXN)
                                   TO  IDXI
      *
                   IF    ( PTZAI-DENPNUM (IDX-ZAI IDXI)
                                       =   ZERO )
                       MOVE    DCACCT-DAYCNT2-GROUP (IDXN)
                                   TO  PTZAI-GROUP (IDX-ZAI IDXI)
                       MOVE    DCACCT-DAY
                                   TO  PTZAI-GROUP-DAY(IDX-ZAI IDXI)
                   END-IF
      *
                   COMPUTE PTZAI-GROUP-KAISU(IDX-ZAI IDXI)
                       =   PTZAI-GROUP-KAISU(IDX-ZAI IDXI)
                       +   DCACCT-DAYCNT2  (IDXN)
      *
               END-IF
      *
           END-PERFORM
      *
           .
       2001111-PTZAI-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報退避処理
      *****************************************************************
       200111-DCSRY-TAIHI-SEC          SECTION.
      *
           PERFORM 900-DCSRY-START-SEC
      *
           MOVE    LOW-VALUE             TO  KEY-SRYACT
           INITIALIZE                        WRK-058-G
           PERFORM UNTIL ( FLG-DCSRY NOT =   ZERO )
                    OR   ( PTSRY-MAX    >=   CONST-PTSRY-MAX )
      *
               COMPUTE PTSRY-MAX   =   PTSRY-MAX   +   1
      *
               IF    ( KEY-SRY-PTID        NOT =   DCSRY-PTID )
                  OR ( KEY-SRY-ZAINUM      NOT =   DCSRY-ZAINUM )
                  OR ( KEY-SRY-ACCTKBN     NOT =   DCSRY-ACCTKBN )
                   MOVE    DCSRY-PTID      TO  KEY-SRY-PTID
                   MOVE    DCSRY-ZAINUM    TO  KEY-SRY-ZAINUM
                   MOVE    DCSRY-ACCTKBN   TO  KEY-SRY-ACCTKBN
                   INITIALIZE                  WRK-058-G
               END-IF
      *
               IF    ( DCSRY-SRYCD (1:3)   =   "058" )
                   INITIALIZE                      WRK-058-G
                   MOVE    DCSRY-SRYCD         TO  WRK-058-SRYCD
                   MOVE    DCSRY-SRYCD         TO  WRK-SRYCD
                   MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( TNS-TANICD          NOT = SPACE )
                    AND  ( TNS-TANICD          NOT = "000" )
                       MOVE    TNS-TANICD  TO  WRK-058-TANICD
                   END-IF
               END-IF
      *
               IF    ( DCSRY-SRYCD (1:1)   =   "7")
                AND  ( DCSRY-MEISKYFLG     =   "1")
                   MOVE    WRK-058-SRYCD
                                   TO  DCSRY-058-SRYCD
                   MOVE    WRK-058-TANICD
                                   TO  DCSRY-058-TANICD
               END-IF
      *
               MOVE    DCSRY-REC   TO  PTSRY-OCC (PTSRY-MAX)
      *
               PERFORM 900-DCSRY-READ-SEC
      *
           END-PERFORM
      *
           .
       200111-DCSRY-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報退避処理
      *****************************************************************
       200112-DCSRY-TAIHI-SEC          SECTION.
      *
           PERFORM 901-DCSRY-START-SEC
      *
           PERFORM UNTIL ( FLG-DCSRY NOT =   ZERO )
                    OR   ( PTSRY-MAX    >=   CONST-PTSRY-MAX )
      *
               COMPUTE PTSRY-MAX   =   PTSRY-MAX   +   1
      *
               MOVE    DCSRY-REC   TO  PTSRY-OCC (PTSRY-MAX)
      *
               PERFORM 901-DCSRY-READ-SEC
      *
           END-PERFORM
      *
           .
       200112-DCSRY-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    主保険チェック処理
      *****************************************************************
       200111-SYUHOKEN-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-JIHI
                                           FLG-RSI-JBI
                                           FLG-KOUGAI
      *
      *    自費・労災・自賠責・公害の判断
           IF    ( COMB-HKNNUM (1:2)   =   "98" OR "90" OR "91")
               MOVE        1           TO  FLG-JIHI
           END-IF
      *
           IF    ( COMB-HKNNUM         =   "971"
                                       OR  "973" )
               MOVE        1           TO  FLG-RSI-JBI
           END-IF
      *
           IF    ( COMB-HKNNUM         =   "975" )
               MOVE        1           TO  FLG-KOUGAI
           END-IF
      *
           .
       200111-SYUHOKEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険ＩＤ退避ワーク編集チェック処理
      *****************************************************************
       200111-THKNID-HEN-CHK-SEC       SECTION.
      *
           IF    ( FLG-JIHI            =   ZERO )
               IF    ( COMB-HKNID  NOT =   ZERO )
                   IF    ( CONST-THKNID-MAX    >   THKNID-MAX )
                       PERFORM 200111-THKNID-HEN-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       200111-THKNID-HEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険ＩＤ退避ワーク編集処理
      *****************************************************************
       200111-THKNID-HEN-SEC           SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   THKNID-MAX )
                    OR   ( IDX0        >   CONST-THKNID-MAX )
                    OR   ( COMB-HKNID  =   THKNID-HKNID (IDX0) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           MOVE    ZERO                TO  THKNID-IDX
      *
           IF    ( IDX0                    >   THKNID-MAX )
               IF    ( CONST-THKNID-MAX    >   THKNID-MAX )
                   COMPUTE THKNID-MAX  =   THKNID-MAX  +   1
                   MOVE    COMB-HKNID  TO  THKNID-HKNID (THKNID-MAX)
                   MOVE    COMB-HKNNUM TO  THKNID-HKNNUM(THKNID-MAX)
      *
                   IF    ( FLG-RSI-JBI =   1 )
                       CONTINUE
                   ELSE
                       MOVE    COMB-HKNID  TO  WRK-HKNID
                       PERFORM 900-PTHKNINF-KEY-SEL-SEC
                       MOVE    PTHKN-HOJOKBN
                                       TO  THKNID-HOJOKBN
                                                        (THKNID-MAX)
                       MOVE    PTHKN-TEKSTYMD
                                       TO  THKNID-TEKSTYMD
                                                        (THKNID-MAX)
                       MOVE    PTHKN-KAKUNINYMD
                                       TO  THKNID-KAKUNINYMD
                                                        (THKNID-MAX)
                   END-IF
      *
                   MOVE    THKNID-MAX  TO  THKNID-IDX
      *
               END-IF
           ELSE
                   MOVE    IDX0        TO  THKNID-IDX
           END-IF
      *
           IF    ( THKNID-IDX          >   ZERO )
               PERFORM VARYING     IDX-DAY FROM    1   BY  1
                       UNTIL     ( IDX-DAY >   31 )
      *
                   IF    ( PTZAI-DAY (IDX-ZAI  IDX-DAY)  >   ZERO )
                       MOVE    1       TO  THKNID-HKNID-DAY
                                                 (THKNID-IDX IDX-DAY)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       200111-THKNID-HEN-EXT.
           EXIT.
      *****************************************************************
      *    公費ＩＤ退避ワーク編集処理
      *****************************************************************
       200111-TKOHID-HEN-CHK-SEC       SECTION.
      *
           IF    ( FLG-RSI-JBI         =   ZERO )
             AND ( FLG-JIHI            =   ZERO )
             AND ( FLG-KOUGAI          =   ZERO )
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL ( IDX3    >  4 )
                       OR    ( COMB-KOHID (IDX3)   =   ZERO   )
                   IF    ( CONST-TKOHID-MAX    >   TKOHID-MAX )
                       PERFORM 200111-TKOHID-HEN-SEC
                   END-IF
                END-PERFORM
           END-IF
      *
           .
       200111-TKOHID-HEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費ＩＤ退避ワーク編集処理
      *****************************************************************
       200111-TKOHID-HEN-SEC           SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   TKOHID-MAX )
                    OR   ( IDX0        >   CONST-TKOHID-MAX )
                    OR   ( COMB-KOHID (IDX3)
                                       =   TKOHID-KOHID (IDX0) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           MOVE    ZERO                TO  TKOHID-IDX
      *
           IF    ( IDX0                    >   TKOHID-MAX )
               IF    ( CONST-TKOHID-MAX    >   TKOHID-MAX)
      *
                   COMPUTE TKOHID-MAX  =   TKOHID-MAX  +   1
      *
                   MOVE    COMB-KOHID (IDX3)
                                       TO  TKOHID-KOHID (TKOHID-MAX)
                   MOVE    COMB-KOHID (IDX3)
                                       TO  WRK-KOHID
                   PERFORM 900-PTKOHINF-KEY-SEL-SEC
                   MOVE    PTKOH-KOHNUM
                                       TO  TKOHID-KOHNUM (TKOHID-MAX)
      *
                   MOVE    TKOHID-MAX  TO  TKOHID-IDX
      *
               END-IF
           ELSE
                   MOVE    IDX0        TO  TKOHID-IDX
           END-IF
      *
           IF    ( TKOHID-IDX          >   ZERO )
               PERFORM VARYING     IDX-DAY FROM    1   BY  1
                       UNTIL     ( IDX-DAY >   31 )
      *
                   IF    ( PTZAI-DAY (IDX-ZAI  IDX-DAY)  >   ZERO )
                       MOVE    1       TO  TKOHID-KOHID-DAY
                                                 (TKOHID-IDX IDX-DAY)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       200111-TKOHID-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ退避ワーク編集処理
      *****************************************************************
       200111-THKNCOMBI-HEN-SEC    SECTION.
      *
           MOVE    ZERO                TO  IDXA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   THKNCOMBI-MAX )
                    OR   ( IDX0        >   CONST-THKNCOMBI-MAX )
                    OR ( ( DCACCT-HKNCOMBI
                                       =   THKNCOMBI-HKNCOMBI (IDX0) )
                      AND( DCACCT-ACCTKBN
                                       =   THKNCOMBI-ACCTKBN  (IDX0) ))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0                    >   THKNCOMBI-MAX )
      *
               IF    ( CONST-THKNCOMBI-MAX >   THKNCOMBI-MAX )
      *
                   COMPUTE THKNCOMBI-MAX   =   THKNCOMBI-MAX  +   1
      *
                   MOVE    DCACCT-HKNCOMBI
                                       TO  THKNCOMBI-HKNCOMBI
                                                       (THKNCOMBI-MAX)
                   MOVE    DCACCT-HKNNUM
                                       TO  THKNCOMBI-HKNNUM
                                                       (THKNCOMBI-MAX)
                   MOVE    DCACCT-KOHNUM
                                       TO  THKNCOMBI-KOHNUM
                                                       (THKNCOMBI-MAX)
                   MOVE    DCACCT-KOHID
                                       TO  THKNCOMBI-KOHID
                                                       (THKNCOMBI-MAX)
                   MOVE    DCACCT-ACCTKBN
                                       TO  THKNCOMBI-ACCTKBN
                                                       (THKNCOMBI-MAX)
                   MOVE    "99999999"  TO  THKNCOMBI-STYMD
                                                       (THKNCOMBI-MAX)
                   MOVE    "00000000"  TO  THKNCOMBI-EDYMD
                                                       (THKNCOMBI-MAX)
                   MOVE    THKNCOMBI-MAX   TO  IDXA
               END-IF
           ELSE
                   MOVE    IDX0            TO  IDXA
           END-IF
      *
           IF    ( IDXA            >   ZERO )
               IF    ( THKNCOMBI-STYMD (IDXA)  >   DCACCT-SRYYMD )
                   MOVE    DCACCT-SRYYMD   TO  THKNCOMBI-STYMD (IDXA)
               END-IF
      *
               IF    ( THKNCOMBI-EDYMD (IDXA)  <   DCACCT-SRYYMD )
                   MOVE    DCACCT-SRYYMD   TO  THKNCOMBI-EDYMD (IDXA)
               END-IF
      *
               PERFORM VARYING     IDX-DAY FROM    1   BY  1
                       UNTIL     ( IDX-DAY >   31 )
      *
                   IF    ( PTZAI-DAY (IDX-ZAI  IDX-DAY)  >   ZERO )
                       MOVE    1       TO  THKNCOMBI-DAY
                                                 (IDXA IDX-DAY)
                   END-IF
      *
               END-PERFORM
      *
           END-IF
      *
           .
       200111-THKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報退避処理
      *****************************************************************
       200111-SRYACT-TAIHI-SEC         SECTION.
      *
           PERFORM VARYING IDX-SRY FROM    1   BY  1
                   UNTIL ( IDX-SRY     >   PTSRY-MAX )
      *
               IF    ( DCACCT-ZAINUM   =   PTSRY-ZAINUM (IDX-SRY) )
                AND  ( DCACCT-ACCTKBN  =   PTSRY-ACCTKBN(IDX-SRY) )
      *
                   MOVE    PTSRY-OCC (IDX-SRY)
                                       TO  DCSRY-REC
      *
      *            処理対象診療行為チェック処理
                   PERFORM 200111-TAISO-SRY-CHK-SEC
      *
                   IF    ( FLG-TAISYO-SRY  =   1 )
      *
                       IF    ( DCSRY-SRYCD (1:1) = "6" OR "S" )
                           EVALUATE    TRUE
                           WHEN  ( SYS-1101-YBKENSAYAKUKBN =   "2" )
                            AND  ( DCACCT-SRYKBN = "40" OR "60"
                                               OR  "64" OR "70" )
      *
                           WHEN  ( SYS-1101-YBSJTYKZKBN    =   "2" )
                            AND  ( DCACCT-SRYKBN           =   "50" )
      *
                           WHEN  ( SYS-1101-YBMSIYKZKBN    =   "2" )
                            AND  ( DCACCT-SRYKBN           =   "54" )
      *
                           WHEN  ( SYS-1101-YBSHOHOYKZKBN  =   "2" )
                            AND  ( DCSRY-SRYSYUKBN =   "213" OR "223"
                                                    OR "233" )
                               CONTINUE
                           WHEN    OTHER
                               IF    ( FLG-TSRYKA-OVER =   ZERO )
                                AND  ( IDX-TSRYKA      >   ZERO )
      *                            診療科退避ワーク診療行為コード編集処理
                                   PERFORM 200111-TSRYKA-SRYCD-HEN-SEC
                               END-IF
                           END-EVALUATE
                       ELSE
                               IF    ( FLG-TSRYKA-OVER =   ZERO )
                                AND  ( IDX-TSRYKA      >   ZERO )
      *                            診療科退避ワーク診療行為コード編集処理
                                   PERFORM 200111-TSRYKA-SRYCD-HEN-SEC
                               END-IF
                       END-IF
      *
                       IF    ( FLG-TSRYKA-OVER =   ZERO )
                        AND  ( IDX-TSRYKA      >   ZERO )
      *                    診療科退避ワーク診療行為コード編集処理
                           PERFORM 200111-TSRYKA-PSRYCD-HEN-SEC
                       END-IF
      *
                       IF    ( DCSRY-SRYCD (1:1) = "S" )
                           CONTINUE
                       ELSE
      *
      *                    点数マスタ検索処理
                           MOVE    DCSRY-SRYCD     TO  WRK-SRYCD
                           MOVE    DCACCT-SRYYMD   TO  WRK-TNS-YMD
                           PERFORM 900-TENSU-KEY-SEL-SEC
      *
                           IF    ( FLG-TSRYKA-OVER =   ZERO )
                            AND  ( IDX-TSRYKA      >   ZERO )
      *                        診療科退避ワーク初診日編集処理
                               PERFORM 200111-TSRYKA-SYOSIN-HEN-SEC
      *                        診療科退避ワーク再診日編集処理
                               PERFORM 200111-TSRYKA-SAISIN-HEN-SEC
      *                        診療科退避ワーク実日数算定日編集処理
                               PERFORM 200111-TSRYKA-JNISSU-HEN-SEC
                           END-IF
      *
      *                    診療行為コード退避ワーク編集処理
                           PERFORM 200111-TSRYCD-HEN-SEC
      *
      *                    労災レセ電チェックファイル編集処理
                           PERFORM 200111-TRSIRECEDEN-HEN-SEC
      *
      *                    減算情報ワーク診療行為退避処理
                           IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
                               PERFORM 200111-TGENZAN-SRY-HEN-SEC
                           END-IF
      *
      *                    実日数退避ワーク編集処理
                           PERFORM 200111-TJNISSU-HEN-SEC
      *
      *                    算定回数退避ワーク編集処理
                           IF    ( TNS-JITUDAY     =   1 )
                            AND  ( TNS-DAYCNT      =   0 )
                               PERFORM 200111-TKAISU-HEN-SEC
                           END-IF
      *
      *                    器材チェック退避ワーク編集処理
                           IF    ( DCSRY-SRYCD (1:1)   =   "7" )
                               PERFORM 200111-TKZI-HEN-SEC
                           END-IF
      *
      *                    介護突合チェック退避領域編集処理
                           PERFORM 200111-TKAIGO-HEN-SEC
      *
                       END-IF
      *
                       EVALUATE    TRUE
                       WHEN  ( DCACCT-SRYKBN   =   "21" OR "22" OR "23")
                        AND  ( DCSRY-SRYSYUKBN(3:1)    NOT =   "3" )
                        AND  ( DCSRY-SRYSYUKBN(3:1)    NOT =   "1" )
                        AND  ( DCSRY-SRYSYUKBN         NOT =   "294" )
                        AND  ( DCACCT-ZAITEN               =   ZERO )
                       WHEN  ( DCSRY-SRYSYUKBN =   "148" OR "149"
                                                OR "213" OR "223"
                                                OR "233" )
                           CONTINUE
                       WHEN   OTHER
                           IF    ( DCSRY-SRYCD =   "810000001" )
                            OR   ( DCSRY-SRYCD(1:2)
                                               =   "83" OR "84" )
                            OR   ( DCSRY-SRYCD(1:4)
                                               =   "0084" )
                               IF  ( DCACCT-ACCTKBN    =   "1" )
      *                            コメントエラー退避ワーク編集処理
                                   PERFORM 200111-TCOMERR-HEN-SEC
                               END-IF
                           END-IF
                       END-EVALUATE
      *
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-SRYACT-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    処理対象診療行為チェック処理
      *****************************************************************
       200111-TAISO-SRY-CHK-SEC        SECTION.
      *
           MOVE    1               TO  FLG-TAISYO-SRY
      *
           MOVE    ZERO            TO  FLG-INGAIZAI
      *
           IF    ( DCACCT-SRYKBN           =   "21" OR "22" OR "23" )
            AND  ( DCSRY-SRYSYUKBN(3:1)    NOT =   "3" )
            AND  ( DCSRY-SRYSYUKBN(3:1)    NOT =   "1" )
            AND  ( DCSRY-SRYSYUKBN         NOT =   "294" )
            AND  ( DCACCT-ZAITEN           =   ZERO )
            AND  ( DCACCT-MINUS            =   ZERO )
               MOVE                1       TO  FLG-INGAISYOHOU
               MOVE                1       TO  FLG-INGAIZAI
           END-IF
      *
      *    薬剤の場合、院外処方の処理対象チェックを行う
           EVALUATE    TRUE
           WHEN  ( DCSRY-SRYSYUKBN     =   "213" OR "223" OR "233" )
               CONTINUE
           WHEN  ( DCACCT-SRYKBN       =   "21" OR "22" OR "23" )
           WHEN  ( DCSRY-SRYSYUKBN     =   "148"  )
               IF    ( DCSRY-SRYCD (1:1) = "6" OR "S" )
                   EVALUATE    ORCBSD01-INGAIPROCKBN
      *            院外処方を含まない
                   WHEN    ZERO
                       IF    ( FLG-INGAIZAI    =   1 )
                           MOVE    ZERO    TO  FLG-TAISYO-SRY
                   END-IF
      *            院外処方を含む
                   WHEN    "1"
                       CONTINUE
      *            院外処方のみ
                   WHEN    "2"
                       IF    ( FLG-INGAIZAI    =   1 )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-TAISYO-SRY
                       END-IF
                   END-EVALUATE
               END-IF
           END-EVALUATE
      *
           .
       200111-TAISO-SRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科退避ワーク診療科編集処理
      *****************************************************************
       200111-TSRYKA-SRYKA-HEN-SEC     SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TSRYKA-MAX )
                    OR   ( IDX0    >   CONST-TSRYKA-MAX )
                    OR   ( DCACCT-SRYKA
                                   =   TSRYKA-SRYKA (IDX0) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0                    >   TSRYKA-MAX )
               IF    ( CONST-TSRYKA-MAX    >   TSRYKA-MAX)
                   COMPUTE TSRYKA-MAX  =   TSRYKA-MAX  +   1
                   MOVE    TSRYKA-MAX      TO  IDX-TSRYKA
                   MOVE    DCACCT-SRYKA    TO  TSRYKA-SRYKA
                                                       (TSRYKA-MAX)
               ELSE
                   MOVE    1           TO  FLG-TSRYKA-OVER
               END-IF
           ELSE
               MOVE    IDX0            TO  IDX-TSRYKA
           END-IF
      *
           .
       200111-TSRYKA-SRYKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科退避ワーク診療行為コード編集処理
      *****************************************************************
       200111-TSRYKA-SRYCD-HEN-SEC     SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TSRYKA-SRYCD-MAX (IDX-TSRYKA) )
                    OR   ( IDX0    >   CONST-TSRYKA-SRYCD-MAX )
                    OR   ( TSRYKA-SRYCD (IDX-TSRYKA IDX0)
                                   =   DCSRY-SRYCD )
      *
               CONTINUE
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  IDX26
      *
           IF    ( IDX0            >   TSRYKA-SRYCD-MAX (IDX-TSRYKA) )
      *
               IF    ( CONST-TSRYKA-SRYCD-MAX
                                   >   TSRYKA-SRYCD-MAX (IDX-TSRYKA) )
      *
                   COMPUTE TSRYKA-SRYCD-MAX (IDX-TSRYKA)
                       =   TSRYKA-SRYCD-MAX (IDX-TSRYKA)  +   1
      *
                   MOVE    TSRYKA-SRYCD-MAX (IDX-TSRYKA)
                                   TO  WRK-TSRYKA-SRYCD-MAX
                   MOVE    DCSRY-SRYCD
                                   TO  TSRYKA-SRYCD
                                       (IDX-TSRYKA WRK-TSRYKA-SRYCD-MAX)
      *
                   MOVE    WRK-TSRYKA-SRYCD-MAX    TO  IDX26
               END-IF
           ELSE
                   MOVE    IDX0                    TO  IDX26
           END-IF
      *
           IF    ( IDX26               >  ZERO )
               PERFORM VARYING IDX27   FROM     1   BY  1
                       UNTIL ( IDX27   >   31 )
                   IF    ( PTZAI-DAY (IDX-ZAI IDX27)  >   ZERO )
                       MOVE    1       TO  TSRYKA-SRYCD-DAY
                                             (IDX-TSRYKA IDX26 IDX27)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200111-TSRYKA-SRYCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科退避ワーク診療行為コード編集処理
      *****************************************************************
       200111-TSRYKA-PSRYCD-HEN-SEC     SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TSRYKA-PSRYCD-MAX (IDX-TSRYKA) )
                    OR   ( IDX0    >   CONST-TSRYKA-PSRYCD-MAX )
                    OR   ( TSRYKA-PSRYCD (IDX-TSRYKA IDX0)
                                   =   DCSRY-SRYCD )
      *
               CONTINUE
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  IDX26
      *
           IF    ( IDX0            >   TSRYKA-PSRYCD-MAX (IDX-TSRYKA) )
      *
               IF    ( CONST-TSRYKA-PSRYCD-MAX
                                   >   TSRYKA-PSRYCD-MAX (IDX-TSRYKA) )
      *
                   COMPUTE TSRYKA-PSRYCD-MAX (IDX-TSRYKA)
                       =   TSRYKA-PSRYCD-MAX (IDX-TSRYKA)  +   1
      *
                   MOVE    TSRYKA-PSRYCD-MAX (IDX-TSRYKA)
                                   TO  WRK-TSRYKA-PSRYCD-MAX
                   MOVE    DCSRY-SRYCD
                                   TO  TSRYKA-PSRYCD
                                      (IDX-TSRYKA WRK-TSRYKA-PSRYCD-MAX)
      *
                   MOVE    WRK-TSRYKA-PSRYCD-MAX    TO  IDX26
               END-IF
           ELSE
                   MOVE    IDX0                    TO  IDX26
           END-IF
      *
           IF    ( IDX26               >  ZERO )
               PERFORM VARYING IDX27   FROM     1   BY  1
                       UNTIL ( IDX27   >   31 )
                   IF    ( PTZAI-DAY (IDX-ZAI IDX27)  >   ZERO )
                       MOVE    1       TO  TSRYKA-PSRYCD-DAY
                                             (IDX-TSRYKA IDX26 IDX27)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200111-TSRYKA-PSRYCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科退避ワーク初診日編集処理
      *****************************************************************
       200111-TSRYKA-SYOSIN-HEN-SEC    SECTION.
      *
           IF   (( TNS-JITUDAY     =   2 )
            AND  ( TNS-DAYCNT      =   1 ))
            OR  (  TNS-SRYCD       =   "111011810"
                                   OR  "111012810"
                                   OR  "101110040"
                                   OR  "111012610"
                                                    )
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   31 )
      *
                   IF    ( PTZAI-DAY (IDX-ZAI IDX2)   >   ZERO )
      *
                       MOVE    1   TO  TSRYKA-ALL-SYOSIN-DAY
                                                 (IDX2)
                       MOVE    1   TO  TSRYKA-SYOSIN-DAY
                                                 (IDX-TSRYKA IDX2)
      *
                       IF    ( TNS-SRYCD       =   "111011810"
                                               OR  "111012810"
                                               OR  "101110040"
                                               OR  "111012610"
                                                               )
                           CONTINUE
                       ELSE
                           MOVE    ORCBSD01-SRYYM
                                               TO  WRK-SYOSIN-CHK-YMD
                           MOVE    IDX2        TO  WRK-SYOSIN-CHK-DD
      *
                           IF    ( TSRYKA-SYOSIN-YMD (IDX-TSRYKA)
                                               =   SPACE )
                               MOVE    WRK-SYOSIN-CHK-YMD
                                               TO  TSRYKA-SYOSIN-YMD
                                                       (IDX-TSRYKA)
                           ELSE
                               IF    ( TSRYKA-SYOSIN-YMD (IDX-TSRYKA)
                                           <   WRK-SYOSIN-CHK-YMD )
                                   MOVE    WRK-SYOSIN-CHK-YMD
                                               TO  TSRYKA-SYOSIN-YMD
                                                       (IDX-TSRYKA)
                               END-IF
                           END-IF
                       END-IF
      *
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       200111-TSRYKA-SYOSIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科退避ワーク再診編集処理
      *****************************************************************
       200111-TSRYKA-SAISIN-HEN-SEC    SECTION.
      *
           IF    ( TNS-JITUDAY     =   2 )
            AND  ( TNS-DAYCNT      =   2 )
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   31 )
      *
                   IF    ( PTZAI-DAY (IDX-ZAI IDX2)   >   ZERO )
      *
                       MOVE    1       TO  TSRYKA-SAISIN-DAY
                                           (IDX-TSRYKA IDX2)
      *
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       200111-TSRYKA-SAISIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科退避ワーク実日数算定日編集処理
      *****************************************************************
       200111-TSRYKA-JNISSU-HEN-SEC    SECTION.
      *
           PERFORM 200111-JNISSU-HEN-SEC
      *
           IF    ( FLG-SHOSAISIN     =    1 )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   31 )
      *
                   IF    ( PTZAI-DAY (IDX-ZAI IDX2)   >   ZERO )
      *
                       MOVE    1       TO  TSRYKA-ALL-JNISSU-DAY
                                           (IDX2)
                       MOVE    1       TO  TSRYKA-JNISSU-DAY
                                           (IDX-TSRYKA IDX2)
      *
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       200111-TSRYKA-JNISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コード退避ワーク編集処理
      *****************************************************************
       200111-TSRYCD-HEN-SEC           SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TSRYCD-MAX )
                    OR   ( IDX0    >   CONST-TSRYCD-MAX )
                    OR   ( TSRYCD-SRYCD (IDX0)
                                   =   DCSRY-SRYCD )
      *
               CONTINUE
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  IDX26
      *
           IF    ( IDX0                    >   TSRYCD-MAX )
               IF    ( CONST-TSRYCD-MAX    >   TSRYCD-MAX )
      *
                   COMPUTE TSRYCD-MAX  =   TSRYCD-MAX  +   1
      *
                   MOVE    DCACCT-NYUGAIKBN
                                   TO  TSRYCD-NYUGAIKBN (TSRYCD-MAX)
                   MOVE    DCSRY-SRYCD
                                   TO  TSRYCD-SRYCD (TSRYCD-MAX)
                   MOVE    DCACCT-ZAIKAISU
                                   TO  TSRYCD-KAISU (TSRYCD-MAX)
                   IF    ( DCACCT-SRYKBN(1:1)  =   "2" OR "3" )
                       MOVE    DCACCT-ZAIKAISU
                                   TO  TSRYCD-YKZKAISU (TSRYCD-MAX)
                   END-IF
                   COMPUTE TSRYCD-SURYOTOTAL (TSRYCD-MAX)
                       =   DCSRY-SURYO *   DCACCT-ZAIKAISU
      *
                   PERFORM VARYING IDX-KANRIRYO FROM   1   BY  1
                           UNTIL ( IDX-KANRIRYO > CONST-TSANTEICD-MAX )
      *
                       IF    ( DCSRY-SRYCD =   CONST-TSANTEICD-SRYCD
                                                        (IDX-KANRIRYO))
                           MOVE    CONST-TSANTEICD-TYPE (IDX-KANRIRYO)
                                   TO  TSRYCD-TYPE (TSRYCD-MAX)
                       END-IF
      *
                   END-PERFORM
      *
                   MOVE    "99999999"  TO  TSRYCD-FSANTEIYMD(TSRYCD-MAX)
      *
                   MOVE    TSRYCD-MAX  TO  IDX26
      *
               END-IF
           ELSE
      *
      *        処理時の入外区分を優先して設定する
               IF    ( DCACCT-NYUGAIKBN    =   ORCBSD01-NYUGAIKBN )
                   MOVE    DCACCT-NYUGAIKBN
                                   TO  TSRYCD-NYUGAIKBN (IDX0)
               END-IF
      *
               COMPUTE TSRYCD-KAISU (IDX0)
                   =   TSRYCD-KAISU (IDX0)
                   +   DCACCT-ZAIKAISU
      *
               IF    ( DCACCT-SRYKBN(1:1)  =   "2" OR "3" )
                   COMPUTE TSRYCD-YKZKAISU (IDX0)
                       =   TSRYCD-YKZKAISU (IDX0)
                       +   DCACCT-ZAIKAISU
               END-IF
      *
               COMPUTE TSRYCD-SURYOTOTAL (IDX0)
                   =   TSRYCD-SURYOTOTAL (IDX0)
                   + ( DCSRY-SURYO *   DCACCT-ZAIKAISU )
      *
               MOVE    IDX0  TO  IDX26
      *
      *
           END-IF
      *
           IF    ( IDX26               >   ZERO )
      *
               MOVE    ZERO            TO  WRK-HKNFLG
      *
               IF    ( DCACCT-JIHOKBN          =   "1"  )
                   IF    ( DCSRY-SRYCD (1:1)   =   "6" )
                       MOVE    1           TO  WRK-HKNFLG
                   END-IF
               ELSE
                   MOVE    PTZAI-HKNNUM (IDX-ZAI)
                                           TO   WRK-ZAIHKNNUM
                   IF    ( PTZAI-KOHNUM (IDX-ZAI)  =   "970" )
                       MOVE    PTZAI-KOHNUM (IDX-ZAI)
                                           TO   WRK-ZAIHKNNUM
                   END-IF
                   PERFORM 2001111-TAISYO-HKN-CHK-SEC
                   IF    ( FLG-TAISYO-HKN      =   1 )
                       MOVE    1           TO  WRK-HKNFLG
                   END-IF
      *
               END-IF
      *
               IF    ( WRK-HKNFLG          =   1 )
                   MOVE    1               TO  TSRYCD-HKNFLG (IDX26)
               END-IF
      *
               IF    ( DCACCT-NYUGAIKBN    =   ORCBSD01-NYUGAIKBN )
                AND  ( WRK-HKNFLG          =   1 )
      *
                   MOVE   1                TO  TSRYCD-TAISYO (IDX26)
      *
                   COMPUTE TSRYCD-KAISU2 (IDX0)
                       =   TSRYCD-KAISU2 (IDX0)
                       +   DCACCT-ZAIKAISU
      *
               END-IF
      *
               PERFORM VARYING IDX27   FROM     1   BY  1
                       UNTIL ( IDX27   >   31 )
                   IF    ( PTZAI-DAY (IDX-ZAI IDX27)  >   ZERO )
                       MOVE    1   TO  TSRYCD-DAY (IDX26 IDX27)
      *
                       IF    ( TSRYCD-DAY-NYUGAIKBN (IDX26 IDX27)
                                                =  SPACE 
                        OR                         DCACCT-NYUGAIKBN )
                           MOVE    DCACCT-NYUGAIKBN
                               TO  TSRYCD-DAY-NYUGAIKBN (IDX26 IDX27)
                       END-IF
      *
      *
                       IF    ( PTZAI-HKNNUM (IDX-ZAI)
                                           =   "971" OR "973" )
                           CONTINUE
                       ELSE
                           MOVE    "1"     TO  TSRYCD-DAY-IHOKBN
                                                          (IDX26 IDX27)
                       END-IF
      *
                       IF    ( WRK-HKNFLG  =   1 )
                           MOVE    1       TO  TSRYCD-DAY-HKNFLG
                                                          (IDX26 IDX27)
                       END-IF
      *
                       COMPUTE TSRYCD-DAY-KAISU (IDX26 IDX27)
                           =   TSRYCD-DAY-KAISU (IDX26 IDX27)
                           +   PTZAI-DAY (IDX-ZAI IDX27)
      *
                       IF    ( TSRYCD-TAISYO (IDX26)   =   1 )
                           COMPUTE TSRYCD-DAY-KAISU2 (IDX26 IDX27)
                               =   TSRYCD-DAY-KAISU2 (IDX26 IDX27)
                               +   PTZAI-DAY (IDX-ZAI IDX27)
                       END-IF
      *
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 2001111-TSRYCD-HEN-SEC
      *
           END-IF
      *
           .
       200111-TSRYCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コード退避ワーク編集処理
      *****************************************************************
       2001111-TSRYCD-HEN-SEC              SECTION.
      *
           PERFORM VARYING IDXI   FROM    1    BY  1
                   UNTIL ( IDXI   >   CONST-GROUP-MAX )
      *
               IF    ( PTZAI-DENPNUM (IDX-ZAI IDXI)
                                          NOT  =   ZERO )
                   IF    ( TSRYCD-DENPNUM (IDX26 IDXI)    =   ZERO )
                       MOVE    PTZAI-GROUP (IDX-ZAI IDXI)
                               TO  TSRYCD-GROUP (IDX26 IDXI)
                       MOVE    PTZAI-GROUP-DAY (IDX-ZAI IDXI)
                               TO  TSRYCD-GROUP-DAY   (IDX26 IDXI)
                   END-IF
                   COMPUTE TSRYCD-GROUP-KAISU (IDX26 IDXI)
                       =   TSRYCD-GROUP-KAISU (IDX26 IDXI)
                       +   PTZAI-GROUP-KAISU (IDX-ZAI IDXI)
      *
                   IF    ( WRK-HKNFLG  =   1 )
                       MOVE    1    TO  TSRYCD-GROUP-HKNFLG
                                                    (IDX26 IDXI)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       2001111-TSRYCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    労災レセ電チェックファイル編集処理
      *****************************************************************
       200111-TRSIRECEDEN-HEN-SEC         SECTION.
      *
           IF    ( DTCHK003-HKNKBN     =   CONST-ROSAI )
            AND  ( ORCBSD01-SRYYM     >=   CONST-H2507 )
            AND  ( FLG-RSI-RECEDEN-CHK =   1 )
            AND  ( SYS-4001-RSIIRYOKBN =   "1" )
            AND  ( DCACCT-SRYKBN       =   "80" )
               MOVE    DCACCT-HKNCOMBI TO  WRK-HKNCOMBI
               PERFORM 220-HKNCOMBI-GET-SEC
               IF    ( COMB-HKNNUM         =   "971" )
                AND  ( WRK-COMB-RSI-HKNKBN =   "1" OR "2" )
                   IF    ( DCSRY-SRYCD (1:5)   =   "09593" )
                       PERFORM 2001111-TRSIRECEDEN-HEN-SEC
                   END-IF
                   IF    ( DCSRY-SRYCD (1:5)       =   "09594" )
                       EVALUATE    TRUE
                       WHEN  ( DCSRY-SRYCD (6:2)   =   "01" OR "02"
                                                    OR "03" OR "04"
                                                    OR "07" OR "08"
                                                    OR "09" OR "10"
                                                    OR "21" OR "22"
                                                    OR "23" OR "24" )
                         AND ( ORCBSD01-SRYYM     >=   CONST-H2804 )
                       WHEN  ( DCSRY-SRYCD (6:2)   =   "01" OR "02"
                                                    OR "03" OR "04"
                                                    OR "07" OR "08"
                                                    OR "09" OR "10" )
                           CONTINUE
                       WHEN    OTHER
                           PERFORM 2001111-TRSIRECEDEN-HEN-SEC
                       END-EVALUATE
                   END-IF
               END-IF
           END-IF
      *
           .
       200111-TRSIRECEDEN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    労災レセ電チェックファイル編集処理
      *****************************************************************
       2001111-TRSIRECEDEN-HEN-SEC        SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TRSIRECEDEN-MAX )
                    OR   ( IDX0    >   CONST-TRSIRECEDEN-MAX )
                    OR   ( TRSIRECEDEN-SRYCD (IDX0)
                                   =   DCSRY-SRYCD )
      *
               CONTINUE
      *
           END-PERFORM
      *
           MOVE    ZERO        TO  IDX26
      *
           IF    ( IDX0                    >   TRSIRECEDEN-MAX )
               IF    ( CONST-TRSIRECEDEN-MAX    >   TRSIRECEDEN-MAX )
      *
                   COMPUTE TRSIRECEDEN-MAX  =   TRSIRECEDEN-MAX  +   1
      *
                   MOVE    DCSRY-SRYCD
                               TO  TRSIRECEDEN-SRYCD (TRSIRECEDEN-MAX)
                   MOVE    TRSIRECEDEN-MAX
                               TO  IDX26
               END-IF
           ELSE
               MOVE    IDX0    TO  IDX26
           END-IF
      *
           IF    ( IDX26           >   ZERO )
               PERFORM VARYING IDX27   FROM     1   BY  1
                       UNTIL ( IDX27   >   31 )
      *
                   IF    ( PTZAI-DAY (IDX-ZAI IDX27)  >   ZERO )
                       MOVE    1       TO  TRSIRECEDEN-DAY (IDX26 IDX27)
                       COMPUTE TRSIRECEDEN-KAISU (IDX26)
                           =   TRSIRECEDEN-KAISU (IDX26)
                           +   PTZAI-DAY (IDX-ZAI IDX27)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       2001111-TRSIRECEDEN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    減算情報ワーク診療行為退避処理
      *****************************************************************
       200111-TGENZAN-SRY-HEN-SEC      SECTION.
      *
           MOVE    "190080570"         TO  TGENZAN-INNAI-SRYCD
           MOVE    "190111690"         TO  TGENZAN-ANZEN-SRYCD
           MOVE    "190111790"         TO  TGENZAN-JOKUSO-SRYCD
      *
           EVALUATE    DCSRY-SRYCD
      *
      *    院内感染防止対策未実施減算
           WHEN    "190080570"
           WHEN    "190799770"
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   31 )
                   IF    ( PTZAI-DAY (IDX-ZAI IDX0)   >   ZERO )
                       IF    ( DCACCT-ACCTKBN  =   "1" )
                           MOVE    1   TO  TGENZAN-INNAI-SRY-DAY (IDX0)
                       ELSE
                           MOVE    1   TO  TGENZAN-INNAI-NSRY-DAY(IDX0)
                       END-IF
                   END-IF
               END-PERFORM
      *
      *    医療安全管理体制未整備減算
           WHEN    "190111690"
           WHEN    "190809090"
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   31 )
                   IF    ( PTZAI-DAY (IDX-ZAI IDX0)   >   ZERO )
                       IF    ( DCACCT-ACCTKBN  =   "1" )
                           MOVE    1   TO  TGENZAN-ANZEN-SRY-DAY (IDX0)
                       ELSE
                           MOVE    1   TO  TGENZAN-ANZEN-NSRY-DAY(IDX0)
                       END-IF
                   END-IF
               END-PERFORM
      *
      *    褥瘡対策未実施減算
           WHEN    "190111790"
           WHEN    "190809190"
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   31 )
                   IF    ( PTZAI-DAY (IDX-ZAI IDX0)   >   ZERO )
                       IF    ( DCACCT-ACCTKBN  =   "1" )
                           MOVE    1   TO  TGENZAN-JOKUSO-SRY-DAY (IDX0)
                       ELSE
                           MOVE    1   TO  TGENZAN-JOKUSO-NSRY-DAY(IDX0)
                       END-IF
                   END-IF
               END-PERFORM
           END-EVALUATE
      *
           .
       200111-TGENZAN-SRY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    実日数退避ワーク編集処理
      *****************************************************************
       200111-TJNISSU-HEN-SEC          SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
      *    実日数算定チェック
           PERFORM 200111-JNISSU-HEN-SEC
      *
           MOVE    COMB-HKNNUM     TO  WRK-HKNNUM
           IF    ( WRK-HKNNUM      =   SPACE )
               MOVE    ZERO        TO  WRK-HKNNUM
           END-IF
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TJNISSU-MAX )
                    OR   ( IDX0    >   CONST-TJNISSU-MAX )
                    OR   ( TJNISSU-HKNNUM (IDX0)
                                   =   WRK-HKNNUM )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0                    >   TJNISSU-MAX )
               IF    ( CONST-TJNISSU-MAX   >   TJNISSU-MAX )
      *
                   COMPUTE TJNISSU-MAX =   TJNISSU-MAX +   1
      *
                   MOVE    WRK-HKNNUM
                                   TO  TJNISSU-HKNNUM (TJNISSU-MAX)
                   MOVE    TJNISSU-MAX
                                   TO  IDXA
               END-IF
           ELSE
               MOVE    IDX0        TO  IDXA
           END-IF
      *
           IF    ( FLG-SHOSAISIN   =   1 )
               IF    ( IDXA            >   ZERO )
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL ( IDX2    >   31 )
                       IF    ( PTZAI-DAY (IDX-ZAI IDX2)   >   ZERO )
                           MOVE    1   TO  TJNISSU-DAY (IDXA IDX2)
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       200111-TJNISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    実日数算定チェック
      *****************************************************************
       200111-JNISSU-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SHOSAISIN
      *
           IF   (( DCACCT-ZAITEN   NOT =   ZERO )
             AND ( DCACCT-ZAIKAISU NOT =   ZERO )
             AND ( DCACCT-SRYKBN       =   "11"
                                       OR  "12" 
                                       OR  "13"
                                       OR  "14" ))
           OR   (( DCACCT-ZAITEN       =   ZERO )
             AND ( DCACCT-ZAIKAISU NOT =   ZERO )
             AND(  DCACCT-SRYKBN       =   "11"
                                      OR   "12"
                                      OR   "99" ))
               IF   (( TNS-JITUDAY     =   2          ) AND
                     ( TNS-DAYCNT      =   1   OR  2  ))
               OR   (  DCSRY-SRYCD     =   "112700850"
                                       OR  "112003350"
                                       OR  "099409901"
                                       OR  "099999902"
                                       OR  "099110001"
                                       OR  "099120001"
      *                        同日再診
                                       OR  "112010150"
                                       OR  "112010250"
                                       OR  "112010350"
                                       OR  "112010450"
                                       OR  "112010550"
                                       OR  "112704450"
                                       OR  "112704550"
                                       OR  "112704650"
                                       OR  "112704750"
                                       OR  "112010650"
                                       OR  "112010750"
                                       OR  "112010850"
                                       OR  "112010950"
                                       OR  "112012850"
                                       OR  "112704850"
                                       OR  "112704950"
                                       OR  "112705050"
                                       OR  "112706850"
                                       OR  "112008850"
                                       OR  "112008950"
                                       OR  "112009050"
                                       OR  "112009150"
                                       OR  "112012550"
                                       OR  "112703450"
                                       OR  "112703550"
                                       OR  "112703650"
                                       OR  "112706550"
                                       OR  "112008350"
                                       OR  "112008450"
                                       OR  "112008550"
                                       OR  "112008650"
                                       OR  "112008750"
                                       OR  "112703050"
                                       OR  "112703150"
                                       OR  "112703250"
                                       OR  "112703350"
      *
                                       OR  "112011710"
                                       OR  "112011810"
                                       OR  "112011910"
                                       OR  "112012010"
                                       OR  "112012210"
                                       OR  "112705850"
                                       OR  "112705950"
                                       OR  "112706050"
                                       OR  "111011810"
                                       OR  "111012810"
                                       OR  "101110040"
                                       OR  "111012610"
                                       OR  "113013910"
                                       OR  "113014010"
                                       OR  "113014110"
                                       OR  "112015810"
                                       OR  "112015950"
                                       OR  "112016210"
                                       OR  "112017610"
                                       OR  "112016410"
                                       OR  "112016550"
                                       OR  "113015810"
                                       OR  "113018410"
      *
                                       OR  "112016850"
                                       OR  "112016950"
                                       OR  "112017010"
                                       OR  "112017150"
                                       OR  "112017450"
      *
                                       OR  "113024810"
                                       OR  "113025710"
                                       OR  "112023210"
                                       OR  "112023450"
                                       OR  "112023750"
                                       OR  "112023550"
                                       OR  "112023850"
      *
                                       OR  "113031310"
                                       OR  "113031410"
                                                           )
               OR   (( DCSRY-SRYCD     =   "101120100") AND
                     ( DCACCT-SRYYMD(1:6) <=  "201306"))
               OR   (( DCSRY-SRYCD     =   "101120040"
                                       OR  "101120050"
                                       OR  "101120060"
                                       OR  "101120070"
                                       OR  "101120080"
                                                      ) AND
                     ( DCACCT-SRYYMD(1:6) >=  "201307"))
      *
                   MOVE    1           TO  FLG-SHOSAISIN
      *
               END-IF
           END-IF
      *
           IF    ( DCACCT-NYUGAIKBN    =   "1" )
               IF    ( DCACCT-ZAITEN   NOT =   ZERO )
                 AND ( DCACCT-ZAIKAISU NOT =   ZERO )
                   MOVE    1           TO  FLG-SHOSAISIN
               END-IF
           END-IF
      *
           IF    ( DCACCT-STS-NYUKHN   =   1 )
            OR   ( DCACCT-STS-GAIHAKU  =   1 )
               MOVE    1               TO  FLG-SHOSAISIN
           END-IF
      *
           .
       200111-JNISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    算定回数退避ワーク編集処理
      *****************************************************************
       200111-TKAISU-HEN-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX-TKAISU
           MOVE    ZERO            TO  FLG-TKAISU-OVER
      *
      *    算定回数退避ワーク保険の種類編集処理
           PERFORM 200111-TKAISU-HKNNUM-HEN-SEC
      *
      *    算定回数退避ワーク診療行為コード編集処理
           IF    ( FLG-TKAISU-OVER =   ZERO )
            AND  ( IDX-TKAISU      >   ZERO )
               PERFORM 200111-TKAISU-SRYCD-HEN-SEC
           END-IF
      *
           .
       200111-TKAISU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    算定回数退避ワーク保険の種類編集処理
      *****************************************************************
       200111-TKAISU-HKNNUM-HEN-SEC    SECTION.
      *
           MOVE    COMB-HKNNUM     TO  WRK-HKNNUM
           IF    ( WRK-HKNNUM      =   SPACE )
               MOVE    ZERO        TO  WRK-HKNNUM
           END-IF
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TKAISU-MAX )
                    OR   ( IDX0    >   CONST-TKAISU-MAX )
                    OR   ( WRK-HKNNUM
                                   =   TKAISU-HKNNUM (IDX0) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0                    >   TKAISU-MAX )
               IF    ( CONST-TKAISU-MAX    >   TKAISU-MAX )
                   COMPUTE TKAISU-MAX  =   TKAISU-MAX  +   1
                   MOVE    TKAISU-MAX  TO  IDX-TKAISU
                   MOVE    WRK-HKNNUM  TO  TKAISU-HKNNUM
                                                   (TKAISU-MAX)
               ELSE
                   MOVE    1           TO  FLG-TKAISU-OVER
               END-IF
           ELSE
               MOVE    IDX0            TO  IDX-TKAISU
           END-IF
      *
           .
       200111-TKAISU-HKNNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    算定回数退避ワーク診療行為コード編集処理
      *****************************************************************
       200111-TKAISU-SRYCD-HEN-SEC    SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TKAISU-SRYCD-MAX (IDX-TKAISU) )
                    OR   ( IDX0    >   CONST-TKAISU-SRYCD-MAX )
                    OR   ( TKAISU-SRYCD (IDX-TKAISU IDX0)
                                   =   DCSRY-SRYCD )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0            >   TKAISU-SRYCD-MAX (IDX-TKAISU) )
      *
               IF    ( CONST-TKAISU-SRYCD-MAX
                                   >   TKAISU-SRYCD-MAX (IDX-TKAISU) )
                   COMPUTE TKAISU-SRYCD-MAX (IDX-TKAISU)
                       =   TKAISU-SRYCD-MAX (IDX-TKAISU)  +   1
                   MOVE    TKAISU-SRYCD-MAX (IDX-TKAISU)
                                   TO  WRK-TKAISU-SRYCD-MAX
                   MOVE    DCSRY-SRYCD
                                   TO  TKAISU-SRYCD
                                       (IDX-TKAISU WRK-TKAISU-SRYCD-MAX)
                   MOVE    WRK-TKAISU-SRYCD-MAX
                                   TO  IDXA
               END-IF
           ELSE
               MOVE    IDX0        TO  IDXA
           END-IF
      *
           IF    ( IDXA            >   ZERO )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   31 )
                   IF    ( PTZAI-DAY (IDX-ZAI IDX2)   >   ZERO )
                       MOVE    1   TO  TKAISU-SRYCD-DAY
                                               (IDX-TKAISU IDXA IDX2)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200111-TKAISU-SRYCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    器材チェック退避ワーク編集処理
      *****************************************************************
       200111-TKZI-HEN-SEC             SECTION.
      *
           IF    ( TNS-TENSIKIBETU     =   0 OR 1 OR 2)
               IF   (  DCSRY-058-TANICD    =   SPACE  OR "000" )
                AND (  TNS-TANICD          =   SPACE  OR "000" )
                   PERFORM 2001111-TKZI-HEN-SEC
               END-IF
           END-IF
      *
           .
       200111-TKZI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    器材チェック退避ワーク編集処理
      *****************************************************************
       200111-TKZI-MICD-HEN-SEC             SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( DCACCT-HKNNUM           =   SPACE )
           WHEN  ( DCACCT-HKNNUM           <   "100" )
            AND  ( DCACCT-HKNNUM       NOT =   "040" )
            AND  ( DCACCT-HKNNUM       NOT =   "068" )
            AND  ( DCACCT-HKNNUM       NOT =   "069" )
           WHEN  ( DCACCT-HKNNUM           =   "971" )
            AND  ( DCACCT-RSI-HKNKBN       =   "1" OR "2" )
      *
               PERFORM 2001111-TKZI-MICD-HEN-SEC
      *
           END-EVALUATE
      *
           .
       200111-TKZI-MICD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    器材チェック退避ワーク編集処理
      *****************************************************************
       2001111-TKZI-HEN-SEC            SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TKZI-MAX )
                    OR   ( IDX0    >   CONST-TKZI-MAX )
                    OR  (( TKZI-SRYCD (IDX0)
                                   =   DCSRY-SRYCD )
                      AND( TKZI-058-SRYCD (IDX0)
                                   =   DCSRY-058-SRYCD ))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0            >   TKZI-MAX )
      *
               IF    ( CONST-TKZI-MAX
                                   >   TKZI-MAX )
                   COMPUTE TKZI-MAX
                       =   TKZI-MAX    +   1
                   MOVE    TKZI-MAX    TO  IDXA
      *
                   MOVE    DCSRY-SRYCD TO  TKZI-SRYCD (IDXA)
                   MOVE    DCSRY-058-SRYCD
                                       TO  TKZI-058-SRYCD (IDXA)
               END-IF
           ELSE
               MOVE    IDX0            TO  IDXA
           END-IF
      *
           IF    ( IDXA            >   ZERO )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   31 )
                   IF    ( PTZAI-DAY (IDX-ZAI IDX2)   >   ZERO )
                       MOVE    1   TO  TKZI-DAY (IDXA IDX2)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       2001111-TKZI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    器材チェック退避ワーク編集処理
      *****************************************************************
       2001111-TKZI-MICD-HEN-SEC       SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TKZI-MICD-MAX )
                    OR   ( IDX0    >   CONST-TKZI-MICD-MAX )
                    OR   ( TKZI-MICD-SRYCD (IDX0)
                                   =   DCSRY-SRYCD )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0            >   TKZI-MICD-MAX )
      *
               IF    ( CONST-TKZI-MICD-MAX
                                   >   TKZI-MICD-MAX )
                   COMPUTE TKZI-MICD-MAX
                       =   TKZI-MICD-MAX    +   1
                   MOVE    TKZI-MICD-MAX    TO  IDXA
      *
                   MOVE    DCSRY-SRYCD TO  TKZI-MICD-SRYCD (IDXA)
               END-IF
           ELSE
               MOVE    IDX0            TO  IDXA
           END-IF
      *
           IF    ( IDXA            >   ZERO )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   31 )
                   IF    ( PTZAI-DAY (IDX-ZAI IDX2)   >   ZERO )
                       MOVE    1   TO  TKZI-MICD-DAY (IDXA IDX2)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       2001111-TKZI-MICD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    介護突合チェック退避領域編集処理
      *****************************************************************
       200111-TKAIGO-HEN-SEC           SECTION.
      *
           IF    ( PTZAI-HKNNUM (IDX-ZAI)
                                       =   "971" OR "973" OR "975" )
               CONTINUE
           ELSE
      *
               MOVE    ZERO            TO  FLG-KAIGO-CHK
      *
               IF    ( DCSRY-SRYCD     =   "114006410"
                                        OR "114015010"
                                        OR "114007810"
                                        OR "114015110"
                                        OR "114007910"
                                        OR "114015210" )
                   MOVE    1           TO  FLG-KAIGO-CHK
               END-IF
      *
               IF    ( PTZAI-HKNNUM (IDX-ZAI)
                                       =   "039" OR "040" )
                   IF    ( DCSRY-SRYCD =   "114015310" )
                       MOVE    1       TO  FLG-KAIGO-CHK
                   END-IF
               END-IF
      *
               IF    ( FLG-KAIGO-CHK   =   1 )
      *
                   MOVE    ZERO        TO  TKAIGO-IDX
                   PERFORM VARYING IDXK    FROM    1   BY  1
                           UNTIL ( IDXK    >   CONST-TKAIGO-MAX )
                            OR   ( TKAIGO-IDX  NOT =   ZERO )
      *
                       IF    ( TKAIGO-SRYCD (IDXK) =   SPACE )
                           MOVE    IDXK    TO  TKAIGO-MAX
                                               TKAIGO-IDX
                           MOVE    DCSRY-SRYCD
                                           TO  TKAIGO-SRYCD (IDXK)
                       ELSE
                           IF    ( TKAIGO-SRYCD (IDXK) =   DCSRY-SRYCD )
                               MOVE    IDXK    TO  TKAIGO-IDX
                           END-IF
                       END-IF
      *
                   END-PERFORM
      *
                   IF    ( TKAIGO-IDX  >   ZERO )
                       PERFORM VARYING IDXD    FROM    1   BY  1
                               UNTIL ( IDXD    >   31 )
      *
                           IF    ( PTZAI-DAY (IDX-ZAI IDXD) >  ZERO )
                               MOVE    1   TO  TKAIGO-SRYCD-DAY
                                                     (TKAIGO-IDX IDXD)
                           END-IF
      *
                       END-PERFORM
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       200111-TKAIGO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    コメントエラー退避ワーク編集処理
      *****************************************************************
       200111-TCOMERR-HEN-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-COMERR
           MOVE    SPACE           TO  WRK-ERRCOM
      *
           IF    ( DCSRY-INPUTNUM  =   ZERO )
               MOVE    1       TO  FLG-COMERR
               IF    ( DCSRY-SRYCD NOT =   "810000001" )
                   MOVE    DCSRY-SRYCD     TO  WRK-SRYCD
                   MOVE    DCACCT-SRYYMD   TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   MOVE    TNS-NAME        TO  WRK-ERRCOM
               END-IF
           ELSE
               PERFORM 900-PTCOM-KEY-SEL-SEC
               IF    ( DCSRY-SRYCD     =   "810000001" )
                   INSPECT PTCOM-INPUTCOMENT REPLACING ALL "　" BY "  "
                   IF    ( PTCOM-INPUTCOMENT  =   SPACE )
                       MOVE    1           TO  FLG-COMERR
                   END-IF
               END-IF
      *
               IF    ( DCSRY-SRYCD(1:2)    =   "83" )
                   MOVE    DCSRY-SRYCD     TO  WRK-SRYCD
                   MOVE    DCACCT-SRYYMD   TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   MOVE    TNS-NAME        TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 800-ZENKAKU-CHK-SEC
                   MOVE    WRK-MAXLEN      TO  WRK-TNSLEN
                   MOVE    PTCOM-INPUTCOMENT
                                           TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 800-ZENKAKU-CHK-SEC
                   MOVE    WRK-MAXLEN      TO  WRK-COMLEN
                   IF    ( WRK-TNSLEN      >=  WRK-COMLEN )
                       MOVE    PTCOM-INPUTCOMENT
                                           TO  WRK-ERRCOM
                       MOVE    1           TO  FLG-COMERR
                   END-IF
               END-IF
      *
               IF    ( DCSRY-SRYCD(1:2)    =   "84" )
                OR   ( DCSRY-SRYCD(1:4)    =   "0084" )
                 IF  ( DCSRY-SRYCD         =   "840000042"
                                           OR  "840000058"
                                           OR  "840000101" )
                   CONTINUE
                 ELSE
                   MOVE    DCSRY-SRYCD     TO  WRK-SRYCD
                   MOVE    DCACCT-SRYYMD   TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   EVALUATE    TRUE
                   WHEN  ( TNS-SSTKIJUNCD (1)  >   ZERO  )
                    AND  ( PTCOM-INPUTCHI (1)  =   SPACE )
                   WHEN  ( TNS-SSTKIJUNCD (3)  >   ZERO  )
                    AND  ( PTCOM-INPUTCHI (2)  =   SPACE )
                   WHEN  ( TNS-SSTKIJUNCD (5)  >   ZERO  )
                    AND  ( PTCOM-INPUTCHI (3)  =   SPACE )
                   WHEN  ( TNS-SSTKIJUNCD (7)  >   ZERO  )
                    AND  ( PTCOM-INPUTCHI (4)  =   SPACE )
                       MOVE    PTCOM-INPUTCOMENT
                                           TO  WRK-ERRCOM
                       MOVE    1           TO  FLG-COMERR
                   END-EVALUATE
                 END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-COMERR          =   1 )
               IF    ( DCSRY-SRYCD =   "810000001" )
                   PERFORM VARYING IDXD    FROM    1   BY  1
                           UNTIL ( IDXD    >   31 )
                       IF    ( PTZAI-DAY(IDX-ZAI IDXD) >   ZERO )
                           MOVE    1   TO  TCOMERR-FREE-DAY (IDXD)
                       END-IF
                   END-PERFORM
               ELSE
                   IF    ( TCOMERR-ZAI-MAX >=  CONST-TCOMERR-ZAI-MAX )
                       CONTINUE
                   ELSE
                       COMPUTE TCOMERR-ZAI-MAX =   TCOMERR-ZAI-MAX +   1
                       MOVE    WRK-ERRCOM      TO  TCOMERR-COM
                                                      (TCOMERR-ZAI-MAX)
                       PERFORM VARYING IDXD    FROM    1   BY  1
                               UNTIL ( IDXD    >   31 )
                           IF    ( PTZAI-DAY(IDX-ZAI IDXD) >   ZERO )
                               MOVE    1   TO  TCOMERR-DAY
                                                 (TCOMERR-ZAI-MAX IDXD)
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-IF
      *
           .
       200111-TCOMERR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算退避ワーク編集処理
      *****************************************************************
       200111-TNYUKSN-HEN-SEC          SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TNYUKSN-MAX )
                    OR   ( DCACCT-HKNCOMBI
                                   =   TNYUKSN-HKNCOMBI (IDX0) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0                    >   TNYUKSN-MAX )
      *
               IF    ( CONST-TNYUKSN-MAX   >   TNYUKSN-MAX )
      *
                   COMPUTE TNYUKSN-MAX
                       =   TNYUKSN-MAX +   1
      *
                   MOVE    DCACCT-HKNCOMBI
                                   TO  TNYUKSN-HKNCOMBI (TNYUKSN-MAX)
                   MOVE    TNYUKSN-MAX
                                   TO  IDXA
               END-IF
           ELSE
               MOVE    IDX0        TO  IDXA
           END-IF
      *
           IF    ( IDXA            >   ZERO )
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   31 )
                   IF    ( PTZAI-DAY (IDX-ZAI IDX2)   >   ZERO )
                       MOVE    1   TO  TNYUKSN-HKNCOMBI-DAY (IDXA IDX2)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200111-TNYUKSN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    算定回数退避ワーク診療行為コード編集処理
      *****************************************************************
       200111-TNYUIN-ACCT-HEN-SEC      SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   31 )
      *
               IF    ( PTZAI-DAY (IDX-ZAI IDX0 )  >   ZERO )
                   MOVE    1       TO  TNYUIN-ACCT-DAY (IDX0)
               END-IF
      *
           END-PERFORM
      *
           .
       200111-TNYUIN-ACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院調剤料算定日退避ワーク編集処理
      *****************************************************************
       200111-TNYUIN-CHOZAI-HEN-SEC    SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   31 )
      *
               IF    ( PTZAI-DAY (IDX-ZAI IDX0 )  >   ZERO )
                   EVALUATE    DCACCT-SRYKBN
                   WHEN    "24"
                       MOVE    1       TO  TNYUIN-CHOZAI-DAY (2 IDX0)
                   WHEN    "27"
                       MOVE    1       TO  TNYUIN-CHOZAI-DAY (3 IDX0)
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           .
       200111-TNYUIN-CHOZAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報退避処理
      *****************************************************************
       200112-SRYINF-TAIHI-SEC         SECTION.
      *
           IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
               MOVE    "2"             TO  WRK-NYUGAIKBN
           ELSE
               MOVE    "1"             TO  WRK-NYUGAIKBN
           END-IF
      *
           PERFORM VARYING IDX-ZAI FROM    1   BY  1
                   UNTIL ( IDX-ZAI >   PTZAI-MAX )
      *
      *
               MOVE    PTZAI-HKNNUM (IDX-ZAI)
                                  TO   WRK-ZAIHKNNUM
               IF    ( PTZAI-KOHNUM (IDX-ZAI)  =   "970" )
                   MOVE    PTZAI-KOHNUM (IDX-ZAI)
                                  TO   WRK-ZAIHKNNUM
               END-IF
               PERFORM 2001111-TAISYO-HKN-CHK-SEC
               IF    ( FLG-TAISYO-HKN  =   ZERO )
                OR   ( PTZAI-NYUGAIKBN (IDX-ZAI)
                                       =   WRK-NYUGAIKBN )
      *
                   PERFORM VARYING IDX-ACCT    FROM    1   BY  1
                           UNTIL ( IDX-ACCT    >   PTACCT-MAX )
                       IF    ( PTZAI-ZAINUM (IDX-ZAI)
                                           =   PTACCT-ZAINUM (IDX-ACCT))
                        AND  ( PTZAI-ACCTKBN(IDX-ZAI)
                                           =   PTACCT-ACCTKBN(IDX-ACCT))
                           MOVE    PTACCT-OCC (IDX-ACCT)
                                           TO  DCACCT-REC
                           IF    ( DCACCT-ACCTRENNUM   =   1 )
                            AND  ( DCACCT-HKNCOMBI NOT =   9999 )
                            AND  ( DCACCT-JIHOKBN  NOT =   "1" )
                               PERFORM 2001121-SRYINF-TAIHI-SEC
                           END-IF
                       END-IF
                   END-PERFORM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200112-SRYINF-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    診療行為情報退避処理
      *****************************************************************
       2001121-SRYINF-TAIHI-SEC        SECTION.
      *
           IF    ( DCACCT-STS-NYUINDAY    =   ZERO )
      *
               PERFORM VARYING IDX-SRY FROM    1   BY  1
                       UNTIL ( IDX-SRY     >   PTSRY-MAX )
      *
                   IF    ( DCACCT-ZAINUM   =   PTSRY-ZAINUM (IDX-SRY))
                    AND  ( DCACCT-ACCTKBN  =   PTSRY-ACCTKBN(IDX-SRY))
      *
                       MOVE    PTSRY-OCC (IDX-SRY)
                                       TO  DCSRY-REC
      *
      *                処理対象診療行為チェック処理
                       PERFORM 200111-TAISO-SRY-CHK-SEC
      *
                       IF    ( FLG-TAISYO-SRY  =   1 )
      *
      *                    点数マスタ検索処理
                           MOVE    DCSRY-SRYCD     TO  WRK-SRYCD
                           MOVE    DCACCT-SRYYMD   TO  WRK-TNS-YMD
                           PERFORM 900-TENSU-KEY-SEL-SEC
      *
      *                    診療行為コード退避ワーク編集処理
                           PERFORM 200111-TSRYCD-HEN-SEC
                       END-IF
      *
                   END-IF
      *
               END-PERFORM
      *
           END-IF
      *
           .
       2001121-SRYINF-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    入院料算定日退避処理
      *****************************************************************
       200111-TNYUINRYO-TAIHI-SEC       SECTION.
      *
           INITIALIZE  ORCSNYUINDAY-AREA
           MOVE    DTCHK003-HOSPNUM    TO  LNK-NYUINDAY-HOSPNUM
           MOVE    DTCHK003-PTID       TO  LNK-NYUINDAY-PTID
           MOVE    ORCBSD01-SRYYM      TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"          USING
                                           ORCSNYUINDAY-AREA
                                           SPA-AREA
           IF    ( LNK-NYUINDAY-RC     NOT =   ZERO )
               INITIALIZE              ORCSNYUINDAY-AREA
           END-IF
           PERFORM VARYING     IDX-DAY    FROM    1   BY  1
                   UNTIL     ( IDX-DAY    >   31 )
      *
               IF    ( LNK-NYUINDAY-KBN (IDX-DAY) =   "1" )
                   MOVE    1       TO  TNYUINRYO-DAY (IDX-DAY)
               END-IF
      *
           END-PERFORM
      *
           MOVE    "1"                     TO  WRK-ZAISKBKBN
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               IF    ( NACCT-SRYCDTOTAL    =    099999916 )
                   CONTINUE
               ELSE
      *
      *            入院料を算定している日にちを退避
                   PERFORM VARYING     IDX-DAY    FROM    1   BY  1
                           UNTIL     ( IDX-DAY    >   31 )
      *
                       IF   ( NACCT-DAY(IDX-DAY)  >   ZERO )
                           MOVE    1       TO  TNYUINRYO-DAY (IDX-DAY)
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
               PERFORM 900-NYUINACCT-KEY17-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200111-TNYUINRYO-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    外泊退避処理
      *****************************************************************
       200111-NACCT-GAIHAKU-TAIHI-SEC   SECTION.
      *
           MOVE    "2"                 TO  WRK-ZAISKBKBN
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
      *
      *    外泊している日にちを退避
           PERFORM VARYING     IDX-DAY FROM    1   BY  1
                   UNTIL     ( IDX-DAY >   31 )
      *
               IF    ( TNYUINRYO-DAY (IDX-DAY)     =   1   )
                   MOVE    NACCT-DAY (IDX-DAY)
                               TO  TNACCT-GAIHAKU-DAY (IDX-DAY)
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200111-NACCT-GAIHAKU-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    入院保険組合せ退避処理
      *****************************************************************
       200111-NACCT-KKN-TAIHI-SEC      SECTION.
      *
           MOVE    DCACCT-STS-NODATA
                                   TO  TNACCT-KKN-NODATA
           MOVE    DCACCT-DAYCNT   TO  TNACCT-KKN-DAY (DCACCT-DAY)
      *
           IF    ( TNACCT-KKN-STDD     =   ZERO )
             OR  ( DCACCT-DAY          <   TNACCT-KKN-STDD )
               MOVE    DCACCT-DAY  TO  TNACCT-KKN-STDD
           END-IF
      *
           IF    ( TNACCT-KKN-EDDD     =   ZERO )
             OR  ( DCACCT-DAY          >   TNACCT-KKN-EDDD )
               MOVE    DCACCT-DAY  TO  TNACCT-KKN-EDDD
           END-IF
      *
           .
       200111-NACCT-KKN-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    データチェック処理
      *****************************************************************
       20011-DATA-CHK-SEC              SECTION.
      *
           OPEN    OUTPUT              DTCHK009-FILE
           CLOSE   DTCHK009-FILE
           OPEN    I-O                 DTCHK009-FILE
      *
      *    データチェック初期処理
           PERFORM 20011-DATA-CHK-INIT-SEC
      *
      *    基本チェック処理
           PERFORM 200111-KIHON-CHK-SEC
           IF    ( FLG-CHKSKIP     =   ZERO )
               PERFORM 200111-DATA-CHK-SEC
           END-IF
      *
           CLOSE   DTCHK009-FILE
      *
           .
       20011-DATA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    データチェック処理
      *****************************************************************
       200111-DATA-CHK-SEC              SECTION.
      *
      *    患者保険情報チェック処理
           PERFORM 200111-PTHKNINF-CHK-SEC
      *
      *    患者公費情報チェック処理
           IF    ( ORCBSD01-CHKKBN (3) NOT =   ZERO  )
               PERFORM 200111-PTKOHINF-CHK-SEC
           END-IF
      *
      *    保険適応年齢チェック
           IF    ( ORCBSD01-CHKKBN (4) NOT =   ZERO  )
               PERFORM 200111-HKNAGE-CHK-SEC
           END-IF
      *
      *
      *    患者病名情報チェック処理
           IF    ( ORCBSD01-CHKKBN (5) NOT =   ZERO  )
            OR   ( ORCBSD01-CHKKBN (6) NOT =   ZERO  )
            OR   ( ORCBSD01-CHKKBN (7) NOT =   ZERO  )
               PERFORM 200111-PTBYOMEI-CHK-SEC
           END-IF
      *
      *    実日数チェック処理
           IF    ( ORCBSD01-CHKKBN(12) NOT =   ZERO  )
               PERFORM 200111-JNISSU-CHK-SEC
           END-IF
      *
      *    患者氏名チェック処理
           IF    ( ORCBSD01-CHKKBN(13) NOT =   ZERO  )
               PERFORM 200111-PTNAME-CHK-SEC
           END-IF
      *
      *    保険組合せチェック処理
           IF    ( ORCBSD01-CHKKBN(14) NOT =   ZERO  )
               PERFORM 200111-HKNCOMBI-CHK-SEC
           END-IF
      *
      *    その他必須チェック処理
           PERFORM 200111-SONOTA-HISSU-CHK-SEC
      *
      *
           .
       200111-DATA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    データチェック処理
      *****************************************************************
       20011-DATA-CHK-CHKMST-SEC       SECTION.
      *
      *    併用算定のチェック用に
      *    入力引数とは反対の入外区分および、対象外の保険で診療会計
      *    テーブルを検索する
           IF    ( ORCBSD01-CHKKBN (11)    NOT =   ZERO )
            OR   ( ORCBSD01-CHKKBN (16)    NOT =   ZERO )
               PERFORM 200112-SRYINF-TAIHI-SEC
           END-IF
      *
           INITIALIZE                  PTACCT-AREA
                                       PTSRY-AREA
                                       PTZAI-AREA
      *
           IF    ( ORCBSD01-CHKKBN (8) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (8)
           END-IF
      *
           IF    ( ORCBSD01-CHKKBN (9) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (9)
           END-IF
      *
      *    (A)
           PERFORM 200113-SRYINF-TAIHI-SEC
      *
      *    レセプトにでないコメント用（上の(A)とセット）
           IF    ( ORCBSD01-CHKKBN (11)    NOT =   ZERO )
            OR   ( ORCBSD01-CHKKBN (16)    NOT =   ZERO )
               PERFORM 200112-SRYINF-TAIHI-SEC
           END-IF
      *
      *    薬剤と病名チェック処理
           IF    ( ORCBSD01-CHKKBN (8) NOT =   ZERO  )
            AND  ( FLG-YKZBYO          =   1 )
               PERFORM 200112-YKZ-BYO-CHK-SEC
           END-IF
      *
      *    診療行為と病名チェック処理
           IF    ( ORCBSD01-CHKKBN (9) NOT =   ZERO  )
            AND  ( FLG-SRYBYO          =   1 )
               PERFORM 200112-SRY-BYO-CHK-SEC
           END-IF
      *
      *    併用算定の診療行為チェック処理
           IF    ( ORCBSD01-CHKKBN(11) NOT =   ZERO  )
               PERFORM 200111-HEI-SRY-CHK-SEC
           END-IF
      *
      *    診療行為どうしの算定漏れ
           IF    ( ORCBSD01-CHKKBN(16) NOT =   ZERO  )
               PERFORM 200111-SRYSNT-CHK-SEC
           END-IF
      *
      *    病名と薬剤
           IF    ( ORCBSD01-CHKKBN(17) NOT =   ZERO  )
               PERFORM 200111-BYOYKZ-CHK-SEC
           END-IF
      *
      *    病名と診療行為
           IF    ( ORCBSD01-CHKKBN(18) NOT =   ZERO  )
               PERFORM 200111-BYOSRY-CHK-SEC
           END-IF
      *
           .
       20011-DATA-CHK-CHKMST-EXT.
           EXIT.
      *****************************************************************
      *    診療情報情報退避処理
      *****************************************************************
       200113-SRYINF-TAIHI-SEC         SECTION.
      *
           PERFORM 200112-DCSRY-TAIHI-SEC
      *
           PERFORM 200111-DCACCT-TAIHI-SEC
      *
           INITIALIZE                  KEY-AREA
      *
           PERFORM VARYING IDX-ZAI FROM    1   BY  1
                   UNTIL ( IDX-ZAI >   PTZAI-MAX )
      *
               IF    ( PTZAI-NYUGAIKBN (IDX-ZAI)
                                       =   ORCBSD01-NYUGAIKBN )
                   PERFORM VARYING IDX-ACCT    FROM    1   BY  1
                           UNTIL ( IDX-ACCT    >   PTACCT-MAX )
                       IF    ( PTZAI-ZAINUM (IDX-ZAI)
                                       =   PTACCT-ZAINUM (IDX-ACCT))
                        AND  ( PTZAI-ACCTKBN(IDX-ZAI)
                                       =   PTACCT-ACCTKBN(IDX-ACCT))
                           MOVE    PTACCT-OCC (IDX-ACCT)
                                   TO  DCACCT-REC
                           IF    ( DCACCT-HKNCOMBI     =   9999 )
                            OR   ( DCACCT-JIHOKBN      =   "1" )
                               CONTINUE
                           ELSE
                               PERFORM 2001111-SRYINF-TAIHI-SEC
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       200113-SRYINF-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    データチェック初期処理
      *****************************************************************
       20011-DATA-CHK-INIT-SEC         SECTION.
      *
           INITIALIZE                  FLG-CHKEND-G
                                       CNT-CHKERR
      *
           .
       20011-DATA-CHK-INIT-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       200111-KIHON-CHK-SEC            SECTION.
      *
           IF    ( FLG-PTINF       NOT =   ZERO )
               MOVE    "990000"    TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 210-ERRMES-HEN-SEC
               MOVE    ALL "1"     TO  FLG-CHKEND-G
               MOVE    1           TO  FLG-CHKSKIP
           END-IF
      *
           .
       200111-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報チェック処理
      *****************************************************************
       200111-PTHKNINF-CHK-SEC         SECTION.
      *
           IF    ( ORCBSD01-CHKKBN (1) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (1)
           END-IF
      *
           IF    ( ORCBSD01-CHKKBN (2) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (2)
           END-IF
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   THKNID-MAX )
      *
               IF    ( THKNID-HKNNUM (IDX4)
                                   =   "971" OR "973" )
                   MOVE    THKNID-HKNID (IDX4)
                                       TO  WRK-HKNID
                   PERFORM 900-PTRSIINF-KEY-SEL-SEC
                   IF    ( PTRSI-SYOCD     =   SPACE )
                    AND  ( PTRSI-RYOSTYMD (1:6)
                                          <= CONST-H2603 )
                       EVALUATE    PTRSI-HKNKBN
                       WHEN    "1"
                           MOVE  PTRSI-KOFUNUM(1:5)
                                       TO  PTRSI-SYOCD
                       WHEN    "2"
                           MOVE    PTRSIINF-REC    TO  TPTRSIINF-REC
                           PERFORM 900-PTRSIINF-KEY4-SEL-SEC
                           IF    ( FLG-PTRSIINF    =   ZERO )
                               IF    ( PTRSI-SYOCD     =   SPACE )
                                   MOVE  PTRSI-KOFUNUM(1:5)
                                       TO  TPTRSI-SYOCD
                               ELSE
                                   MOVE    PTRSI-SYOCD
                                       TO  TPTRSI-SYOCD
                               END-IF
                           END-IF
                           MOVE    TPTRSIINF-REC   TO  PTRSIINF-REC
                       END-EVALUATE
                   END-IF
      *
      *            労災・自賠責保険情報チェック
                   IF    ( ORCBSD01-SRYYM  >=      CONST-H2507 )
                       PERFORM 200111-PTRSIINF-CHK-SEC
                   END-IF
      *
               ELSE
      *            患者保険情報検索処理
                   MOVE    THKNID-HKNID (IDX4)
                                       TO  WRK-HKNID
                   PERFORM 900-PTHKNINF-KEY-SEL-SEC
      *
      *            保険者番号抜けチェック
                   IF    ( FLG-CHKEND (1)          =   ZERO )
                       PERFORM 200111-HKNNUM-CHK-SEC
                   END-IF
      *
      *            被保険者記号・番号抜け
                   IF    ( FLG-CHKEND (2)          =   ZERO )
                       PERFORM 200111-KIGONUM-CHK-SEC
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-PTHKNINF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者労災・自賠責保険情報チェック
      *****************************************************************
       200111-PTRSIINF-CHK-SEC         SECTION.
      *
           IF    ( PTRSI-HKNKBN            =   "1" OR "2" )
            AND  ( PTRSI-RYOSTYMD (1:6)    =   ORCBSD01-SRYYM )
               IF    ( PTRSI-SYOCD         =   SPACE )
                   MOVE    PTRSI-HKNNUM    TO  SRH-HKNNUM
                   MOVE    "990028"        TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 211-ERRMES-HEN-SEC
               ELSE
                   PERFORM 900-LABOR-SIO-KEY-SEL-SEC
                   IF    ( FLG-LABOR-SIO   NOT =   ZERO )
                       MOVE    PTRSI-HKNNUM    TO  SRH-HKNNUM
                       MOVE    PTRSI-SYOCD     TO  WRK-MAE-INPUT
                       PERFORM 800-ZENKAKU-HEN-SEC
                       MOVE    WRK-OUT-INPUT   TO  WRK-ERRMES-PLUS
                       MOVE    "990029"        TO  WRK-ERRCD
                       INITIALIZE              TERRINF-AREA
                       PERFORM 216-ERRMES-HEN-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       200111-PTRSIINF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号抜けチェック
      *****************************************************************
       200111-HKNNUM-CHK-SEC           SECTION.
      *
           IF    ( PTHKN-HKNNUM        =   "975" )
               CONTINUE
           ELSE
               IF    ( PTHKN-HKNJANUM      =   SPACE )
      *
                   MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
                   MOVE    "010001"        TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 211-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (1) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (1)
                       GO  TO  200111-HKNNUM-CHK-EXT
                   END-IF
      *
               END-IF
           END-IF
      *
           .
       200111-HKNNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    被保険者記号・番号抜けチェック
      *****************************************************************
       200111-KIGONUM-CHK-SEC          SECTION.
      *
           MOVE    PTHKN-HKNJANUM      TO  SRH-1102-HKNJANUM
           MOVE    PTHKN-HKNNUM        TO  SRH-1102-HKNNUM
           PERFORM 2111-CK4-1102-GET-SEC
      *
           MOVE    PTHKN-KIGO          TO  WRK-PTHKN-KIGO
           MOVE    PTHKN-NUM           TO  WRK-PTHKN-NUM
      *
           INSPECT WRK-PTHKN-KIGO      REPLACING   ALL "　"    BY  "  "
           INSPECT WRK-PTHKN-NUM       REPLACING   ALL "　"    BY  "  "
      *
           EVALUATE    TRUE
           WHEN  ( SRH-1102-KGCHKPTN   =   "0" )
             AND ( WRK-PTHKN-KIGO      =   SPACE )
               MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
               MOVE    "020001"        TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 211-ERRMES-HEN-SEC
               IF    ( ORCBSD01-CHKKBN (2) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (2)
                   GO  TO  200111-KIGONUM-CHK-EXT
               END-IF
           WHEN  ( SRH-1102-KGCHKPTN   =   "2" )
            AND  ( WRK-PTHKN-KIGO  NOT =   SPACE )
               MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
               MOVE    "020012"        TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 211-ERRMES-HEN-SEC
               IF    ( ORCBSD01-CHKKBN (2) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (2)
                   GO  TO  200111-KIGONUM-CHK-EXT
               END-IF
           END-EVALUATE
      *
           IF  ( WRK-PTHKN-NUM       =   SPACE )
      *
               MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
               MOVE    "020005"        TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 211-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (2) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (2)
                   GO  TO  200111-KIGONUM-CHK-EXT
               END-IF
      *
           END-IF
      *
           IF    ( PTHKN-KIGO          NOT =   SPACE )
               MOVE    PTHKN-KIGO      TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF  ( FLG-KANACHK       NOT =   ZERO )
      *
                   MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
                   IF    ( FLG-KANACHK =   2 )
                       MOVE    "020006"    TO  WRK-ERRCD
                   ELSE
                       MOVE    "020002"    TO  WRK-ERRCD
                   END-IF
                   INITIALIZE              TERRINF-AREA
                   PERFORM 211-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (2) =   "1" )
                       MOVE    1       TO  FLG-CHKEND (2)
                       GO  TO  200111-KIGONUM-CHK-EXT
                   END-IF
               ELSE
                   IF    ( SRH-1102-KGCHKPTN    =   "2" )
                    AND  ( WRK-PTHKN-KIGO  NOT =   SPACE )
                       CONTINUE
                   ELSE
                       PERFORM 2001111-HKN-KIGO-CHK-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF    ( PTHKN-NUM           NOT =   SPACE )
               MOVE    PTHKN-NUM       TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF  ( FLG-KANACHK       NOT =   ZERO )
      *
                   MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
                   IF    ( FLG-KANACHK =   2 )
                       MOVE    "020007"    TO  WRK-ERRCD
                   ELSE
                       MOVE    "020003"    TO  WRK-ERRCD
                   END-IF
      *
                   INITIALIZE              TERRINF-AREA
                   PERFORM 211-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (2) =   "1" )
                       MOVE    1       TO  FLG-CHKEND (2)
                       GO  TO  200111-KIGONUM-CHK-EXT
                   END-IF
               ELSE
                   PERFORM 2001111-HKN-NUM-CHK-SEC
               END-IF
           END-IF
      *
      *    レセ電の場合、記号と文字列長チェック
           IF    ( FLG-RECEDEN-CHK     =   1 )
      *
               MOVE    SPACE               TO  WRK-STR
               STRING  WRK-PTHKN-KIGO      DELIMITED   BY  SPACE
               INTO    WRK-STR
               END-STRING
      *
               IF    ( WRK-PTHKN-KIGO      NOT =   WRK-STR )
                   MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
                   MOVE    "020008"        TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 211-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (2) =   "1" )
                       MOVE    1       TO  FLG-CHKEND (2)
                       GO  TO  200111-KIGONUM-CHK-EXT
                   END-IF
               END-IF
      *
               MOVE    SPACE               TO  WRK-KIGONUM-STR
               STRING  PTHKN-KIGO              DELIMITED   BY  SPACE
                       PTHKN-NUM               DELIMITED   BY  SPACE
               INTO    WRK-KIGONUM-STR
               END-STRING
      *
               IF    ( WRK-KIGONUM-STR (39:)   NOT =   SPACE )
                   MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
                   MOVE    "020004"        TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 211-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (2) =   "1" )
                       MOVE    1       TO  FLG-CHKEND (2)
                       GO  TO  200111-KIGONUM-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           .
       200111-KIGONUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険別記号番号チェック処理
      *****************************************************************
       2001111-HKN-KIGO-CHK-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNKIGOERR
      *
           INITIALIZE                  SDTCHK04AREA
           MOVE    PTHKN-KIGO      TO  SDTCHK04-KIGONUM
           CALL    "ORCSDTCHK04"   USING   SDTCHK04AREA
                                           SPA-AREA
      *
           IF    ( SRH-1102-KGSUJI         =   "2" )
               IF    ( SDTCHK04-SUJI       =   1 )
                   MOVE    1       TO  FLG-HKNKIGOERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-KGKANJI        =   "2" )
               IF    ( SDTCHK04-KANJI     =   1 )
                   MOVE    1       TO  FLG-HKNKIGOERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-KGABT          =   "2" )
               IF    ( SDTCHK04-ALPHABET   =   1 )
                   MOVE    1       TO  FLG-HKNKIGOERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-KGHYPN         =   "2" )
               IF    ( SDTCHK04-HYPHEN     =   1 )
                OR   ( SDTCHK04-TYOON      =   1 )
                   MOVE    1       TO  FLG-HKNKIGOERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-KGKAKKO        =   "2" )
               IF    ( SDTCHK04-KAKKO      =   1 )
                   MOVE    1       TO  FLG-HKNKIGOERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-KGETC          =   "2" )
               IF    ( SDTCHK04-ETC        =   1 )
                   MOVE    1       TO  FLG-HKNKIGOERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-KGKANJINASHI   =   "2" )
               IF    ( SDTCHK04-KANJI      =   ZERO )
                   MOVE    1       TO  FLG-HKNKIGOERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-KGKETAMIN  NOT =   SPACE )
               IF    ( SRH-1102-KGKETAMIN  >   SDTCHK04-KETA )
                   MOVE    1       TO  FLG-HKNKIGOERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-KGKETAMAX  NOT =   SPACE )
               IF    ( SRH-1102-KGKETAMAX  <   SDTCHK04-KETA )
                   MOVE    1       TO  FLG-HKNKIGOERR
               END-IF
           END-IF
      *
           IF    ( FLG-HKNKIGOERR      =   1 )
               MOVE    "020009"    TO  WRK-ERRCD
               PERFORM 2001111-HKN-KIGO-ERR-SET-SEC
           END-IF
      *
           .
       2001111-HKN-KIGO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険別記号チェック処理
      *****************************************************************
       2001111-HKN-KIGO-ERR-SET-SEC    SECTION.
      *
           MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
           INITIALIZE              TERRINF-AREA
           PERFORM 211-ERRMES-HEN-SEC
           IF    ( ORCBSD01-CHKKBN (2) =   "1" )
               MOVE    1       TO  FLG-CHKEND (2)
           END-IF
      *
           MOVE        1       TO  FLG-HKNKIGOERR
      *
           .
       2001111-HKN-KIGO-ERR-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険別番号チェック処理
      *****************************************************************
       2001111-HKN-NUM-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNNUMERR
      *
           INITIALIZE                  SDTCHK04AREA
           MOVE    PTHKN-NUM(1:2)  TO  SDTCHK04-KIGONUM
           CALL    "ORCSDTCHK04"   USING   SDTCHK04AREA
                                           SPA-AREA
      *
           IF    ( SRH-1102-NM1SUJI        =   "2" )
               IF    ( SDTCHK04-SUJI       =   1 )
                   MOVE    1       TO  FLG-HKNNUMERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-NM1KANJI       =   "2" )
               IF    ( SDTCHK04-KANJI      =   1 )
                   MOVE    1       TO  FLG-HKNNUMERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-NM1ABT         =   "2" )
               IF    ( SDTCHK04-ALPHABET   =   1 )
                   MOVE    1       TO  FLG-HKNNUMERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-NM1HYPN        =   "2" )
               IF    ( SDTCHK04-HYPHEN     =   1 )
                OR   ( SDTCHK04-TYOON      =   1 )
                   MOVE    1       TO  FLG-HKNNUMERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-NM1KAKKO       =   "2" )
               IF    ( SDTCHK04-KAKKO      =   1 )
                   MOVE    1       TO  FLG-HKNNUMERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-NM1ETC         =   "2" )
               IF    ( SDTCHK04-ETC        =   1 )
                   MOVE    1       TO  FLG-HKNNUMERR
               END-IF
           END-IF
      *
           INITIALIZE                  SDTCHK04AREA
           MOVE    PTHKN-NUM       TO  SDTCHK04-KIGONUM
           CALL    "ORCSDTCHK04"   USING   SDTCHK04AREA
                                           SPA-AREA
      *
           IF    ( SRH-1102-NMSUJINASHI    =   "2" )
               IF    ( SDTCHK04-SUJI       =   ZERO )
                   MOVE    2       TO  FLG-HKNNUMERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-NMKETAMIN  NOT =   SPACE )
               IF    ( SRH-1102-NMKETAMIN  >   SDTCHK04-KETA )
                   MOVE    2       TO  FLG-HKNNUMERR
               END-IF
           END-IF
      *
           IF    ( SRH-1102-NMKETAMAX  NOT =   SPACE )
               IF    ( SRH-1102-NMKETAMAX  <   SDTCHK04-KETA )
                   MOVE    2       TO  FLG-HKNNUMERR
               END-IF
           END-IF
      *
           IF    ( FLG-HKNNUMERR  =   1 )
               MOVE    "020010"    TO  WRK-ERRCD
               PERFORM 2001111-HKN-NUM-ERR-SET-SEC
           END-IF
           IF    ( FLG-HKNNUMERR  =   2 )
               MOVE    "020011"    TO  WRK-ERRCD
               PERFORM 2001111-HKN-NUM-ERR-SET-SEC
           END-IF
      *
      *
           IF    ( FLG-HKNNUMERR       =   ZERO )
            AND  ( PTHKN-NUM (3:)  NOT =   SPACE )
      *
               INITIALIZE                  SDTCHK04AREA
               MOVE    PTHKN-NUM (3:)  TO  SDTCHK04-KIGONUM
               CALL    "ORCSDTCHK04"   USING   SDTCHK04AREA
                                               SPA-AREA
      *
               IF    ( SRH-1102-NM2SUJI        =   "2" )
                   IF    ( SDTCHK04-SUJI       =   1 )
                       MOVE    1       TO  FLG-HKNNUMERR
                   END-IF
               END-IF
      *
               IF    ( SRH-1102-NM2KANJI       =   "2" )
                   IF    ( SDTCHK04-KANJI      =   1 )
                       MOVE    1       TO  FLG-HKNNUMERR
                   END-IF
               END-IF
      *
               IF    ( SRH-1102-NM2ABT         =   "2" )
                   IF    ( SDTCHK04-ALPHABET   =   1 )
                       MOVE    1       TO  FLG-HKNNUMERR
                   END-IF
               END-IF
      *
               IF    ( SRH-1102-NM2HYPN        =   "2" )
                   IF    ( SDTCHK04-HYPHEN     =   1 )
                    OR   ( SDTCHK04-TYOON      =   1 )
                       MOVE    1       TO  FLG-HKNNUMERR
                   END-IF
               END-IF
      *
               IF    ( SRH-1102-NM2KAKKO       =   "2" )
                   IF    ( SDTCHK04-KAKKO      =   1 )
                       MOVE    1       TO  FLG-HKNNUMERR
                   END-IF
               END-IF
      *
               IF    ( SRH-1102-NM2ETC         =   "2" )
                   IF    ( SDTCHK04-ETC        =   1 )
                       MOVE    1       TO  FLG-HKNNUMERR
                   END-IF
               END-IF
      *
               IF    ( FLG-HKNNUMERR  =   1 )
                   MOVE    "020011"    TO  WRK-ERRCD
                   PERFORM 2001111-HKN-NUM-ERR-SET-SEC
               END-IF
      *
           END-IF
      *
           .
       2001111-HKN-NUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険別番号チェック処理
      *****************************************************************
       2001111-HKN-NUM-ERR-SET-SEC     SECTION.
      *
           MOVE    PTHKN-HKNNUM    TO  SRH-HKNNUM
           INITIALIZE              TERRINF-AREA
           PERFORM 211-ERRMES-HEN-SEC
           IF    ( ORCBSD01-CHKKBN (2) =   "1" )
               MOVE    1       TO  FLG-CHKEND (2)
           END-IF
      *
           MOVE        1       TO  FLG-HKNNUMERR
      *
           .
       2001111-HKN-NUM-ERR-SET-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報チェック処理
      *****************************************************************
       200111-PTKOHINF-CHK-SEC         SECTION.
      *
           IF    ( ORCBSD01-CHKKBN (3) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (3)
           END-IF
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   TKOHID-MAX )
      *
      *        患者公費情報検索処理
               MOVE    TKOHID-KOHID (IDX4)
                                       TO  WRK-KOHID
               PERFORM 900-PTKOHINF-KEY-SEL-SEC
      *
      *        負担者番号・受給者番号抜け
               IF    ( FLG-CHKEND (3)          =   ZERO )
      *
      *            第三者行為、長期以外を対象とする
                   IF    ( PTKOH-KOHNUM NOT = "970" AND "972" AND "974")
                        PERFORM 200111-FTNJKYNUM-CHK-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-CHKEND (3)  =   1 )
                   MOVE    TKOHID-MAX  TO  IDX4
               END-IF
      *
           END-PERFORM
      *
           .
       200111-PTKOHINF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担者番号・受給者番号抜けチェック
      *****************************************************************
       200111-FTNJKYNUM-CHK-SEC        SECTION.
      *
           MOVE    PTKOH-KOHNUM        TO  SRH-1040-KOHNUM
           PERFORM 2111-SYS1040-GET-SEC
      *
           IF    ( PTKOH-KOHNUM        =   "012" OR "025" )
               PERFORM 900-MONTHLYNUM-KEY-SEL-SEC
               IF    ( MONTHLYNUM-JKYSNUM  NOT =   SPACE )
                   MOVE    MONTHLYNUM-JKYSNUM
                                       TO  PTKOH-JKYSNUM
               END-IF
           END-IF
      *
           MOVE    PTKOH-FTNJANUM      TO  WRK-PTKOH-FTNJANUM
           MOVE    PTKOH-JKYSNUM       TO  WRK-PTKOH-JKYSNUM
      *
           INSPECT WRK-PTKOH-FTNJANUM  REPLACING   ALL "　"    BY  "  "
           INSPECT WRK-PTKOH-JKYSNUM   REPLACING   ALL "　"    BY  "  "
      *
           IF    ( SRH-1040-FTNJANUMCHK    =   "1" )
               CONTINUE
           ELSE
               IF    ( WRK-PTKOH-FTNJANUM  =   SPACE )
      *
                   MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                   MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                   MOVE    "030001"        TO  WRK-ERRCD
                   MOVE    SPACE           TO  WRK-ERRMES-PLUS
                   INITIALIZE              TERRINF-AREA
                   PERFORM 212-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (3) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (3)
                       GO  TO  200111-FTNJKYNUM-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SRH-1040-JKYSNUMCHK =   "0" OR SPACE )
               IF    ( WRK-PTKOH-JKYSNUM   =   SPACE )
                   MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                   MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                   MOVE    "030002"        TO  WRK-ERRCD
                   MOVE    SPACE           TO  WRK-ERRMES-PLUS
                   INITIALIZE              TERRINF-AREA
                   PERFORM 212-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (3) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (3)
                       GO  TO  200111-FTNJKYNUM-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SRH-1040-JKYSNUMCHK =   "2"  )
               IF    ( WRK-PTKOH-JKYSNUM   NOT =   SPACE )
                   MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                   MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                   MOVE    "030013"        TO  WRK-ERRCD
                   MOVE    SPACE           TO  WRK-ERRMES-PLUS
                   INITIALIZE              TERRINF-AREA
                   PERFORM 212-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (3) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (3)
                       GO  TO  200111-FTNJKYNUM-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( PTKOH-FTNJANUM      NOT =   SPACE )
      *
               IF    ( SRH-1040-FTNJHANKAKUCHK =   "2" )
                   MOVE    PTKOH-FTNJANUM      TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 800-HANKAKU-CHK-SEC
                   IF  ( FLG-KANACHK       =   ZERO )
                       MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                       MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                       MOVE    "030007"        TO  WRK-ERRCD
                       MOVE    SPACE           TO  WRK-ERRMES-PLUS
                       INITIALIZE              TERRINF-AREA
                       PERFORM 212-ERRMES-HEN-SEC
                   END-IF
               END-IF
               IF    ( SRH-1040-FTNJZENKAKUCHK =   "2" )
                   MOVE    PTKOH-FTNJANUM      TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 800-ZENKAKU-CHK-SEC
                   IF  ( FLG-KANACHK       =   ZERO )
                       MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                       MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                       MOVE    "030008"        TO  WRK-ERRCD
                       MOVE    SPACE           TO  WRK-ERRMES-PLUS
                       INITIALIZE              TERRINF-AREA
                       PERFORM 212-ERRMES-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    PTKOH-FTNJANUM  TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-HANKAKU-CHK-SEC
               IF  ( FLG-KANACHK       =   ZERO )
                   IF    ( SRH-1040-FTNJKETACHK    =   "0" )
                       IF    ( WRK-MAXLEN  >=    SRH-1040-FTNJKETAMIN )
                        AND  ( WRK-MAXLEN  <=    SRH-1040-FTNJKETAMAX )
                           CONTINUE
                       ELSE
                           MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                           MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                           MOVE    "030011"        TO  WRK-ERRCD
                           MOVE    SPACE           TO  WRK-ERRMES-PLUS
                           INITIALIZE              TERRINF-AREA
                           PERFORM 212-ERRMES-HEN-SEC
                       END-IF
                   END-IF
               ELSE
                   PERFORM 800-ZENKAKU-CHK-SEC
                   IF  ( FLG-KANACHK   =   ZERO )
                       IF    ( SRH-1040-FTNJKETACHK    =   "0" )
                           IF    ( WRK-MAXLEN
                                     >=    SRH-1040-FTNJKETAMIN * 2)
                            AND  ( WRK-MAXLEN
                                     <=    SRH-1040-FTNJKETAMAX * 2 )
                               CONTINUE
                           ELSE
                               MOVE    PTKOH-KOHNUM  TO  SRH-KOHNUM
                               MOVE    PTKOH-PAYKBN  TO  SRH-PAYKBN
                               MOVE    "030011"      TO  WRK-ERRCD
                               MOVE    SPACE         TO  WRK-ERRMES-PLUS
                               INITIALIZE                TERRINF-AREA
                               PERFORM 212-ERRMES-HEN-SEC
                           END-IF
                       END-IF
                   ELSE
      *
                       MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                       MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                       IF  ( FLG-KANACHK   =   2 )
                           MOVE    "030005"    TO  WRK-ERRCD
                       ELSE
                           MOVE    "030011"    TO  WRK-ERRCD
                       END-IF
                       MOVE    SPACE           TO  WRK-ERRMES-PLUS
                       INITIALIZE              TERRINF-AREA
                       PERFORM 212-ERRMES-HEN-SEC
      *
                       IF    ( ORCBSD01-CHKKBN (3) =   "1" )
                           MOVE    1           TO  FLG-CHKEND (3)
                           GO  TO  200111-FTNJKYNUM-CHK-EXT
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( SRH-1040-FTNJNUMERICCHK =   "2" )
                   MOVE    "gsub"      TO  ORCSSTR-COMMAND
                   MOVE    PTKOH-FTNJANUM
                                       TO  ORCSSTR-STRING1
                   MOVE    "A"         TO  ORCSSTR-STRING2
                   MOVE    "B"         TO  ORCSSTR-STRING3
                   CALL    "orcsstring" USING  ORCSSTRINGAREA
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   10 )
                       MOVE    "gsub"  TO  ORCSSTR-COMMAND
                       MOVE    CONST-NUM (IDX0)
                                       TO  ORCSSTR-STRING2
                       MOVE    "A"     TO  ORCSSTR-STRING3
                       CALL    "orcsstring" USING  ORCSSTRINGAREA
                       MOVE    "gsub"  TO  ORCSSTR-COMMAND
                       MOVE    CONST-ZENNUM (IDX0)
                                       TO  ORCSSTR-STRING2
                       MOVE    "AA"    TO  ORCSSTR-STRING3
                       CALL    "orcsstring" USING  ORCSSTRINGAREA
                   END-PERFORM
                   INSPECT ORCSSTR-STRING1 REPLACING ALL "A" BY  " "
                   IF    ( ORCSSTR-STRING1 =   SPACE )
                       CONTINUE
                   ELSE
                       MOVE    PTKOH-KOHNUM  TO  SRH-KOHNUM
                       MOVE    PTKOH-PAYKBN  TO  SRH-PAYKBN
                       MOVE    "030011"      TO  WRK-ERRCD
                       MOVE    SPACE         TO  WRK-ERRMES-PLUS
                       INITIALIZE                TERRINF-AREA
                       PERFORM 212-ERRMES-HEN-SEC
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( PTKOH-JKYSNUM       NOT =   SPACE )
               IF    ( SRH-1040-JKYSHANKAKUCHK =   "2" )
                   MOVE    PTKOH-JKYSNUM   TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 800-HANKAKU-CHK-SEC
                   IF  ( FLG-KANACHK       =   ZERO )
                       MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                       MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                       MOVE    "030009"        TO  WRK-ERRCD
                       MOVE    SPACE           TO  WRK-ERRMES-PLUS
                       INITIALIZE              TERRINF-AREA
                       PERFORM 212-ERRMES-HEN-SEC
                   END-IF
               END-IF
               IF    ( SRH-1040-JKYSZENKAKUCHK =   "2" )
                   MOVE    PTKOH-JKYSNUM   TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 800-ZENKAKU-CHK-SEC
                   IF  ( FLG-KANACHK       =   ZERO )
                       MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                       MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                       MOVE    "030010"        TO  WRK-ERRCD
                       MOVE    SPACE           TO  WRK-ERRMES-PLUS
                       INITIALIZE              TERRINF-AREA
                       PERFORM 212-ERRMES-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    PTKOH-JKYSNUM   TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-HANKAKU-CHK-SEC
               IF  ( FLG-KANACHK       =   ZERO )
                   IF    ( SRH-1040-JKYSKETACHK    =   "0" )
                       IF    ( WRK-MAXLEN  >=    SRH-1040-JKYSKETAMIN )
                        AND  ( WRK-MAXLEN  <=    SRH-1040-JKYSKETAMAX )
                           CONTINUE
                       ELSE
                           MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                           MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                           MOVE    "030012"        TO  WRK-ERRCD
                           MOVE    SPACE           TO  WRK-ERRMES-PLUS
                           INITIALIZE              TERRINF-AREA
                           PERFORM 212-ERRMES-HEN-SEC
                       END-IF
                   END-IF
               ELSE
                   PERFORM 800-ZENKAKU-CHK-SEC
                   IF  ( FLG-KANACHK   =   ZERO )
                       IF    ( SRH-1040-JKYSKETACHK    =   "0" )
                           IF    ( WRK-MAXLEN
                                     >=    SRH-1040-JKYSKETAMIN * 2)
                            AND  ( WRK-MAXLEN
                                     <=    SRH-1040-JKYSKETAMAX * 2 )
                               CONTINUE
                           ELSE
                               MOVE    PTKOH-KOHNUM  TO  SRH-KOHNUM
                               MOVE    PTKOH-PAYKBN  TO  SRH-PAYKBN
                               MOVE    "030012"      TO  WRK-ERRCD
                               MOVE    SPACE         TO  WRK-ERRMES-PLUS
                               INITIALIZE                TERRINF-AREA
                               PERFORM 212-ERRMES-HEN-SEC
                           END-IF
                       END-IF
                   ELSE
      *
                       MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                       MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                       IF  ( FLG-KANACHK   =   2 )
                           MOVE    "030006"    TO  WRK-ERRCD
                       ELSE
                           MOVE    "030012"    TO  WRK-ERRCD
                       END-IF
                       MOVE    SPACE           TO  WRK-ERRMES-PLUS
                       INITIALIZE              TERRINF-AREA
                       PERFORM 212-ERRMES-HEN-SEC
      *
                       IF    ( ORCBSD01-CHKKBN (3) =   "1" )
                           MOVE    1           TO  FLG-CHKEND (3)
                           GO  TO  200111-FTNJKYNUM-CHK-EXT
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( SRH-1040-JKYSNUMERICCHK =   "2" )
                   MOVE    "gsub"      TO  ORCSSTR-COMMAND
                   MOVE    PTKOH-JKYSNUM
                                       TO  ORCSSTR-STRING1
                   MOVE    "A"         TO  ORCSSTR-STRING2
                   MOVE    "B"         TO  ORCSSTR-STRING3
                   CALL    "orcsstring" USING  ORCSSTRINGAREA
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   10 )
                       MOVE    "gsub"  TO  ORCSSTR-COMMAND
                       MOVE    CONST-NUM (IDX0)
                                       TO  ORCSSTR-STRING2
                       MOVE    "A"     TO  ORCSSTR-STRING3
                       CALL    "orcsstring" USING  ORCSSTRINGAREA
                       MOVE    "gsub"  TO  ORCSSTR-COMMAND
                       MOVE    CONST-ZENNUM (IDX0)
                                       TO  ORCSSTR-STRING2
                       MOVE    "AA"    TO  ORCSSTR-STRING3
                       CALL    "orcsstring" USING  ORCSSTRINGAREA
                   END-PERFORM
                   INSPECT ORCSSTR-STRING1 REPLACING ALL "A" BY  " "
                   IF    ( ORCSSTR-STRING1 =   SPACE )
                       CONTINUE
                   ELSE
                       MOVE    PTKOH-KOHNUM    TO  SRH-KOHNUM
                       MOVE    PTKOH-PAYKBN    TO  SRH-PAYKBN
                       MOVE    "030012"        TO  WRK-ERRCD
                       MOVE    SPACE           TO  WRK-ERRMES-PLUS
                       INITIALIZE              TERRINF-AREA
                       PERFORM 212-ERRMES-HEN-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       200111-FTNJKYNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険適応年齢チェック処理
      *****************************************************************
       200111-HKNAGE-CHK-SEC           SECTION.
      *
           IF    ( ORCBSD01-CHKKBN (4) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (4)
           END-IF
      *
      *    ７０歳到達月算出
           MOVE    PTINF-BIRTHDAY      TO  WRK-AGE70
           IF    ( WRK-AGE70-DD        >   1 )
               IF    ( WRK-AGE70-MM    =   12 )
                   COMPUTE WRK-AGE70-YY    =   WRK-AGE70-YY    +   1
                   MOVE    1           TO  WRK-AGE70-MM
               ELSE
                   COMPUTE WRK-AGE70-MM    =   WRK-AGE70-MM    +   1
               END-IF
                   MOVE    1           TO  WRK-AGE70-DD
           END-IF
           COMPUTE WRK-AGE70-YY        =   WRK-AGE70-YY    +   70
      *
           PERFORM VARYING IDX20   FROM    1   BY  1
                   UNTIL ( IDX20   >   THKNCOMBI-MAX )
                    OR   ( FLG-CHKEND (4)  =   1 )
      *
               MOVE    THKNCOMBI-HKNCOMBI (IDX20)
                                       TO  WRK-HKNCOMBI
      *
               MOVE    WRK-KJNYMD      TO  WRK-YMD
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( THKNCOMBI-DAY (IDX20 IDX-DAY)   =   1  )
                       MOVE    IDX-DAY TO  WRK-DD
                       MOVE    31      TO  IDX-DAY
                   END-IF
               END-PERFORM
      *
               MOVE    WRK-YMD         TO  WRK-SRYYMD
      *
               PERFORM 220-HKNCOMBI-GET-SEC
      *
               IF    ( COMB-HKNID          =   ZERO )
                OR   ( COMB-HKNNUM         =   "971" )
                OR   ( COMB-HKNNUM         =   "973" )
                OR   ( COMB-HKNNUM         =   "975" )
                OR   ( COMB-KOHHKNNUM (1)  =   "970" )
                OR   ( COMB-HKNNUM (1:2)   =   "98" OR "90" OR "91" )
                OR  (( COMB-HKNNUM         =   "002") 
                 AND ( COMB-HOJOKBN        >=  "A" )
                 AND ( COMB-HOJOKBN        <=  "I" ))
                   CONTINUE
               ELSE
      *
                   MOVE    THKNCOMBI-ACCTKBN  (IDX20)
                                       TO  WRK-ACCTKBN
      *
                   PERFORM 200112-HKNAGE-CHK-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-HKNAGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険適応年齢チェック処理
      *****************************************************************
       200112-HKNAGE-CHK-SEC           SECTION.
      *
      *    年齢計算
           INITIALIZE ORCSAGECHKAREA
           MOVE    DTCHK003-HOSPNUM     TO  AGECHK-HOSPNUM
           MOVE    DTCHK003-PTID       TO  AGECHK-PTID
           MOVE    WRK-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           MOVE    COMB-HKNCOMBINUM    TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
           IF    ( AGECHK-RC       =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE  ORCSAGECHKAREA
           END-IF
      *
           MOVE    ZERO                TO  FLG-SEIHO
           MOVE    ZERO                TO  FLG-RJN
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >       4 )
                    OR   ( COMB-KOHID (IDX0)
                                           =   ZERO )
      *
                IF    ( COMB-KOHHKNNUM (IDX0)
                                           =   "012" OR "025" )
                    MOVE   1           TO  FLG-SEIHO
                END-IF
      *
                IF    ( COMB-KOHHKNNUM (IDX0)
                                           =   "027" OR "968" )
                    MOVE   1           TO  FLG-RJN
                END-IF
      *
           END-PERFORM
      *
           IF    ( COMB-HKNNUM     =   "039" OR "040" )
                    MOVE   1           TO  FLG-RJN
           END-IF
      *
           IF    ( AGECHK-NENREI       <   15 )
            AND  ( COMB-HONKZKKBN     =   "1" )
               MOVE    COMB-HKNNUM     TO  SRH-HKNNUM
               MOVE    "040001"        TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 200112-HKNAGE-ERRMES-HEN-SEC
               INITIALIZE              TERRINF-AREA
               PERFORM 215-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (4) =   "1" )
                   MOVE    1       TO  FLG-CHKEND (4)
                   GO  TO  200112-HKNAGE-CHK-EXT
               END-IF
           END-IF
      *
           IF    ( FLG-RJN         =   1 )
               IF    ( AGECHK-ROUJIN   NOT = 1 )
                   MOVE    COMB-HKNNUM     TO  SRH-HKNNUM
                   MOVE    "040002"        TO  WRK-ERRCD
                   PERFORM 200112-HKNAGE-ERRMES-HEN-SEC
                   INITIALIZE              TERRINF-AREA
                   PERFORM 215-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (4) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (4)
                       GO  TO  200112-HKNAGE-CHK-EXT
                   END-IF
               END-IF
      *
               IF    ( AGECHK-RJNCHK       =   3 )
                   MOVE    COMB-HKNNUM     TO  SRH-HKNNUM
                   MOVE    "040004"        TO  WRK-ERRCD
                   PERFORM 200112-HKNAGE-ERRMES-HEN-SEC
                   INITIALIZE              TERRINF-AREA
                   PERFORM 215-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (4) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (4)
                       GO  TO  200112-HKNAGE-CHK-EXT
                   END-IF
               END-IF

      *
           END-IF
      *
           IF    ( FLG-RJN             =   ZERO )
            AND  ( FLG-SEIHO           =   ZERO )
               IF    ( AGECHK-ROUJIN   =   1 )
                   MOVE    COMB-HKNNUM     TO  SRH-HKNNUM
                   MOVE    "040003"        TO  WRK-ERRCD
                   PERFORM 200112-HKNAGE-ERRMES-HEN-SEC
                   INITIALIZE              TERRINF-AREA
                   PERFORM 215-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (4) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (4)
                       GO  TO  200112-HKNAGE-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-CHKEND (4)      =   ZERO )
               IF    ( ORCBSD01-SRYYM >=   CONST-H2004 )
                   IF    ( COMB-HKNNUM       =  "067" )
                    AND  ( AGECHK-NENREI    >=  65 )
                       MOVE    COMB-HKNNUM     TO  SRH-HKNNUM
                       MOVE    "040005"        TO  WRK-ERRCD
                       INITIALIZE              TERRINF-AREA
                       PERFORM 211-ERRMES-HEN-SEC
      *
                       IF    ( ORCBSD01-CHKKBN (4) =   "1" )
                           MOVE    1           TO  FLG-CHKEND (4)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-RJN             =   ZERO )
            AND  ( AGECHK-NENREI      >=   70 )
               PERFORM VARYING IDX41   FROM    1   BY  1
                       UNTIL ( IDX41   >   THKNID-MAX )
      *
                   IF    ( THKNID-HKNNUM (IDX41)(1:1)    =   "9" )
                    OR   ( THKNID-HKNNUM (IDX41) =   "039" OR "040" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-AGE70ERR
                       IF    ( THKNID-HOJOKBN (IDX41)    =   SPACE )
                           MOVE    1   TO  FLG-AGE70ERR
                       END-IF
                       IF    ( THKNID-TEKSTYMD (IDX41) >=  WRK-AGE70 )
                        OR   ( THKNID-KAKUNINYMD(IDX41)>=  WRK-AGE70 )
                           CONTINUE
                       ELSE
                           MOVE    1   TO  FLG-AGE70ERR
                       END-IF
                       IF    ( FLG-AGE70ERR    =   1 )
                           MOVE    THKNID-HKNNUM (IDX41)
                                               TO  SRH-HKNNUM
                           MOVE    "040006"    TO  WRK-ERRCD
                           INITIALIZE              TERRINF-AREA
                           PERFORM 211-ERRMES-HEN-SEC
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200112-HKNAGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険適応年齢エラーメッセージ編集処理
      *****************************************************************
       200112-HKNAGE-ERRMES-HEN-SEC    SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMES-PLUS
      *
           IF    ( WRK-ACCTKBN         =   "1" )
               MOVE    "診療会計："    TO  WRK-STR
           ELSE
               MOVE    "入院会計："    TO  WRK-STR
           END-IF
      *
           STRING  WRK-STR         DELIMITED   BY  SPACE
                   WRK-HKNCOMBIMEI DELIMITED   BY  SPACE
           INTO    WRK-ERRMES-PLUS
           END-STRING
      *
           .
       200112-HKNAGE-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者病名情報チェック処理
      *****************************************************************
       200111-PTBYOMEI-CHK-SEC         SECTION.
      *
      *    患者病名情報チェック初期処理
           PERFORM 200111-PTBYOMEI-CHK-INIT-SEC
      *
           IF    ( FLG-CHKEND (5) =   ZERO )
             OR  ( FLG-CHKEND (6) =   ZERO )
             OR  ( FLG-CHKEND (7) =   ZERO )
               IF    ( TSRYKA-MAX          >   ZERO )
                   PERFORM 2001111-PTBYOMEI-CHK-SEC
               END-IF
           END-IF
      *
           .
       200111-PTBYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者病名情報チェック処理
      *****************************************************************
       2001111-PTBYOMEI-CHK-SEC        SECTION.
      *
           INITIALIZE              FLG-PTBYO-DAY-G
                                   FLG-PTBYO-DAY-KA-G
                                   FLG-PTBYO-DAY2-G
                                   FLG-PTBYO-DAY2-KA-G
                                   WRK-SYOSINYMD-G
      *
           IF    ( FLG-CHKEND (6)  =   ZERO )
      *        初診算定日取得処理
               PERFORM 2001121-SYOSIN-SANTEI-SEC
      *
      *        初再診期間編集処理
               PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL ( IDX4    >   TSRYKA-MAX )
      *
                   PERFORM 200112-SYOSAISIN-KKN-SEC
      *
               END-PERFORM
           END-IF
      *
      *    患者病名情報チェック処理
           PERFORM 200112-PTBYOMEI-CHK-SEC
      *
      *    患者病名登録チェック
           IF    ( FLG-CHKEND (5)      =   ZERO )
      *
               IF    ( SYS-1101-BYOSRYKAKBN    =   "1" OR SPACE )
      *
                   PERFORM VARYING IDX4    FROM    1   BY  1
                           UNTIL ( IDX4    >   TSRYKA-MAX )
                       MOVE    TSRYKA-PTBYO-CNT1 (IDX4)
                                       TO  CNT-PTBYOMEI1
                       MOVE    TSRYKA-PTBYO-CNT2 (IDX4)
                                       TO  CNT-PTBYOMEI2
                       MOVE    TSRYKA-PTSYUBYO-CNT (IDX4)
                                       TO  CNT-PTSYUBYO
                       MOVE    TSRYKA-SRYKA (IDX4)
                                       TO  WRK-SRYKA
                       MOVE    TSRYKA-HKNTG-CNT (IDX4)
                                       TO  CNT-HKNTG
                       PERFORM 2001121-PTBYO-TOUROKU-CHK-SEC
                   END-PERFORM
      *
               ELSE
                       MOVE    SPACE       TO  WRK-SRYKA
                       PERFORM 2001121-PTBYO-TOUROKU-CHK-SEC
               END-IF
           END-IF
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   TSRYKA-MAX )
                    OR   ( FLG-CHKEND (6)  =   1 )
      *
      *        診療開始日チェック（２）処理
               IF    ( FLG-CHKEND (6)  =   ZERO )
                   PERFORM 200112-PTBYO-SRYYMD-CHK-SEC
               END-IF
      *
      *        診療開始日チェック（３）処理
               IF    ( FLG-CHKEND (6)  =   ZERO )
                   MOVE    ZERO        TO  WRK-TIYU-CHUSHI
                   MOVE    "060005"    TO  WRK-SAISHIN-ERRCD
                   PERFORM 200112-PTBYO-SAISIN-CHK-SEC
               END-IF
      *
      *        診療開始日チェック（３）処理
               IF    ( FLG-CHKEND (6)  =   ZERO )
                   MOVE    1           TO  WRK-TIYU-CHUSHI
                   MOVE    "060003"    TO  WRK-SAISHIN-ERRCD
                   PERFORM 200112-PTBYO-SAISIN-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       2001111-PTBYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者病名情報チェック初期処理
      *****************************************************************
       200111-PTBYOMEI-CHK-INIT-SEC    SECTION.
      *
           IF    ( ORCBSD01-CHKKBN (5) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (5)
           END-IF
      *
           IF    ( ORCBSD01-CHKKBN (6) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (6)
           END-IF
      *
           IF    ( ORCBSD01-CHKKBN (7) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (7)
           END-IF
      *
      *    疑い病名開始日チェック用年月
           MOVE    SPACE               TO  WRK-UTAGAI-CHK-YM
           MOVE    WRK-KJNYMD          TO  WRK-SYMD
           MOVE    "2"                 TO  WRK-ZOGENPTN
           IF    ( SYS-1101-UTAGAIKKN-X    IS  NUMERIC )
               COMPUTE WRK-ZOGEN
                   =   SYS-1101-UTAGAIKKN  *   -1
           ELSE
               MOVE    -2              TO  WRK-ZOGEN
           END-IF
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-SYMD (1:6)      TO  WRK-UTAGAI-CHK-YM
      *
      *    急性病名開始日チェック用年月
           MOVE    SPACE               TO  WRK-KYUSEI-CHK-YM
           MOVE    WRK-KJNYMD          TO  WRK-SYMD
           MOVE    "2"                 TO  WRK-ZOGENPTN
           IF    ( SYS-1101-KYUSEIKKN-X    IS  NUMERIC )
               IF    ( SYS-1101-KYUSEIKKN  >   ZERO )
                   COMPUTE WRK-ZOGEN
                       =   SYS-1101-KYUSEIKKN  *   -1
                   PERFORM 800-CALENDAR-SEC
                   MOVE    WRK-SYMD (1:6)  TO  WRK-KYUSEI-CHK-YM
               END-IF
           END-IF
      *
           .
       200111-PTBYOMEI-CHK-INIT-EXT.
           EXIT.
      *****************************************************************
      *    患者病名情報チェック処理
      *****************************************************************
       200112-PTBYOMEI-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  CNT-PTBYOMEI1
                                           CNT-PTBYOMEI2
                                           CNT-HKNTG
                                           CNT-PTSYUBYO
      *
           MOVE    ZERO                TO  FLG-CHK-END
      *
           PERFORM 900-PTBYOMEI-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
                    OR   ( FLG-CHK-END     NOT =   ZERO )
      *
               PERFORM 2001121-TPTBYOEX-EDIT-SEC
      *
      *        患者病名エラーメッセージ付加情報編集
               MOVE    PTBYO-BYOMEI    TO  WRK-PTBYOMEI
               MOVE    PTBYO-SRYYMD    TO  WRK-PTBYOSRYYMD
               PERFORM 2001121-PTBYOMES-PLUS-SEC
      *
               PERFORM 200113-TAISYO-BYOMEI-CHK-SEC
               IF    ( FLG-TAISYO-BYOMEI   =   1   )
                   PERFORM 200113-PTBYOMEI-CHK-SEC
               END-IF
      *
               IF    ( PTBYO-SRYYMD (1:6) <=  ORCBSD01-SRYYM )
                   PERFORM VARYING IDX4    FROM    1   BY  1
                           UNTIL ( IDX4    >   TSRYKA-MAX )
                       PERFORM 2001121-SAISIN-DAY-HEN-SEC
                   END-PERFORM
               END-IF
      *
               IF    ( FLG-CHKEND (5) =   1 )
                 AND ( FLG-CHKEND (6) =   1 )
                 AND ( FLG-CHKEND (7) =   1 )
                   MOVE    1           TO  FLG-CHK-END
               END-IF
      *
               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
               MOVE    "key6"          TO  MCP-PATHNAME
               PERFORM 900-PTBYOMEI-FETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200112-PTBYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    対象病名チェック処理
      *****************************************************************
       200113-TAISYO-BYOMEI-CHK-SEC    SECTION.
      *
           MOVE    1                   TO  FLG-TAISYO-BYOMEI
      *
      *    レセプト表示期間を過ぎていないかチェック
           IF    ( PTBYO-REZEMM    >   ZERO )
      *
               MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
      *
               IF    ( PTBYO-REZEMM    >   1 )
                   MOVE    "2"         TO  WRK-ZOGENPTN
                   COMPUTE WRK-ZOGEN   =   PTBYO-REZEMM
                                               -   1
                   PERFORM 800-CALENDAR-SEC
               END-IF
      *
               IF    ( WRK-KJNYMD(1:6) >   WRK-SYMD(1:6))
                   MOVE    ZERO        TO  FLG-TAISYO-BYOMEI
               END-IF
      *
           END-IF
      *
      *    レセプト表示を行うか
           IF    ( PTBYO-REZEFLG   =   "1" )
                   MOVE    ZERO        TO  FLG-TAISYO-BYOMEI
           END-IF
      *
      *    転帰に移行が設定されていないか
           IF    ( PTBYO-TENKIKBN  =   "8" )
            AND  ( PTBYO-TENKIYMD (1:6) 
                                  <=   ORCBSD01-SRYYM )
                   MOVE    ZERO        TO  FLG-TAISYO-BYOMEI
           END-IF
      *
           IF    ( PTBYO-HKNCOMBI  NOT =   ZERO )
            OR   ( PTBYO-KOHID     NOT =   ZERO )
      *
               MOVE    "99999999"          TO  WRK-BYOEDYMD
               IF    ( PTBYO-TENKIYMD  NOT =   SPACE )
                   MOVE    PTBYO-TENKIYMD  TO  WRK-BYOEDYMD
               END-IF
      *
               IF    ( PTBYO-HKNCOMBI  NOT =   ZERO )
                   MOVE    PTBYO-HKNCOMBI  TO  WRK-HKNCOMBI
                   PERFORM 220-HKNCOMBI-GET-SEC
                   MOVE    COMB-HKNNUM     TO  WRK-ZAIHKNNUM
                   PERFORM 2001111-TAISYO-HKN-CHK-SEC
                   IF    ( FLG-TAISYO-HKN  =   ZERO )
                       MOVE    ZERO        TO  FLG-TAISYO-BYOMEI
                   END-IF
      *
                   MOVE    ZERO            TO  WRK-HKNCOMBI
                   PERFORM VARYING IDX-COMB    FROM    1   BY  1
                           UNTIL ( IDX-COMB    >   THKNCOMBI-MAX )
                            OR   ( WRK-HKNCOMBI    NOT =   ZERO )
      *
                       IF    ( COMB-HKNCOMBINUM
                                           =   THKNCOMBI-HKNCOMBI
                                                        (IDX-COMB))
                           MOVE    THKNCOMBI-HKNCOMBI (IDX-COMB)
                                           TO  WRK-HKNCOMBI
      *
                           IF    ( PTBYO-SRYYMD    <=  COMB-TEKEDYMD )
                            AND  ( WRK-BYOEDYMD    >=  COMB-TEKSTYMD )
                               CONTINUE
                           ELSE
                               MOVE    ZERO
                                           TO  FLG-TAISYO-BYOMEI
                           END-IF
      *
                       END-IF
      *
                   END-PERFORM
      *
                   IF    ( WRK-HKNCOMBI    =   ZERO )
                           MOVE    ZERO        TO  FLG-TAISYO-BYOMEI
                   END-IF
               END-IF
               IF    ( PTBYO-KOHID     NOT =   ZERO )
                   MOVE    "970"           TO  WRK-ZAIHKNNUM
                   PERFORM 2001111-TAISYO-HKN-CHK-SEC
                   IF    ( FLG-TAISYO-HKN  =   ZERO )
                       MOVE    ZERO        TO  FLG-TAISYO-BYOMEI
                   ELSE
                       MOVE    PTBYO-KOHID TO  WRK-KOHID
                       PERFORM 900-PTKOHINF-KEY-SEL-SEC
                       IF    ( PTBYO-SRYYMD    <=  PTKOH-TEKEDYMD )
                        AND  ( WRK-BYOEDYMD    >=  PTKOH-TEKSTYMD )
                           CONTINUE
                       ELSE
                           MOVE    ZERO
                                           TO  FLG-TAISYO-BYOMEI
                       END-IF
                   END-IF
               END-IF
           ELSE
               IF    ( DTCHK003-HKNKBN =   CONST-ROSAI
                                       OR  CONST-JIBAI 
                                       OR  CONST-KOUGAI
                                       OR  CONST-DAISAN     )
                   MOVE    ZERO        TO  FLG-TAISYO-BYOMEI
               END-IF
           END-IF
      *
           .
       200113-TAISYO-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者病名情報チェック処理
      *****************************************************************
       200113-PTBYOMEI-CHK-SEC         SECTION.
      *
           IF    ( SYS-1101-BYOSRYKAKBN    =   "1" OR SPACE )
               PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL ( IDX4    >   TSRYKA-MAX )
      *
                   IF    ( PTBYO-SRYKA =   TSRYKA-SRYKA (IDX4) )
      *
                       PERFORM 2001121-PTBYOMEI-CHK-SEC
      *
                       IF    ( PTBYO-NYUGAIKBN
                                           =   SPACE )
                        OR   ( PTBYO-NYUGAIKBN
                                           =   ORCBSD01-NYUGAIKBN )
      *
                           PERFORM 2001121-PTBYO-DAY-HEN-SEC
      *
      *                    診療開始日チェック（１）
                           IF    ( FLG-CHKEND (6)  =   ZERO )
                               PERFORM 2001121-PTBYO-SRYYMD-CHK-SEC
                           END-IF
                       END-IF
      *
                       MOVE    TSRYKA-MAX  TO  IDX4
      *
                   END-IF
      *
               END-PERFORM
           ELSE
                   PERFORM 2001121-PTBYOMEI-CHK-SEC
      *
                   IF    ( PTBYO-NYUGAIKBN =   SPACE )
                    OR   ( PTBYO-NYUGAIKBN =   ORCBSD01-NYUGAIKBN )
      *
                       PERFORM 2001121-PTBYO-DAY-HEN-SEC
      *
      *                診療開始日チェック（１）
                       IF    ( FLG-CHKEND (6)  =   ZERO )
                           PERFORM 2001121-PTBYO-SRYYMD-CHK-SEC
                       END-IF
                   END-IF
           END-IF
      *
           .
       200113-PTBYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者病名情報チェック処理
      *****************************************************************
       2001121-PTBYOMEI-CHK-SEC        SECTION.
      *
           IF    ( SYS-1101-BYOSRYKAKBN    =   "1" OR SPACE )
               COMPUTE TSRYKA-PTBYO-CNT1 (IDX4)
                   =   TSRYKA-PTBYO-CNT1 (IDX4)    +   1
           ELSE
               COMPUTE CNT-PTBYOMEI1
                   =   CNT-PTBYOMEI1   +   1
           END-IF
      *
      *    患者病名対象チェック
           PERFORM 2001121-PTBYO-TAISYO-CHK-SEC
      *
           IF    ( FLG-PTBYO-TAISYO    =   1 )
      *
               IF    ( SYS-1101-BYOSRYKAKBN    =   "1" OR SPACE )
                   COMPUTE TSRYKA-PTBYO-CNT2 (IDX4)
                       =   TSRYKA-PTBYO-CNT2 (IDX4)    +   1
                   IF    ( PTBYO-SYUBYOFLG     =   "1" )
                       COMPUTE TSRYKA-PTSYUBYO-CNT (IDX4)
                           =   TSRYKA-PTSYUBYO-CNT (IDX4)    +   1
                   END-IF
                   IF    ( TPTBYOEX-HKNTGKBN   =   "1" )
                       COMPUTE TSRYKA-HKNTG-CNT (IDX4)
                           =   TSRYKA-HKNTG-CNT (IDX4)    +   1
                   END-IF
               ELSE
                   COMPUTE CNT-PTBYOMEI2
                       =   CNT-PTBYOMEI2   +   1
                   IF    ( PTBYO-SYUBYOFLG     =   "1" )
                       COMPUTE CNT-PTSYUBYO
                           =   CNT-PTSYUBYO    +   1
                   END-IF
                   IF    ( TPTBYOEX-HKNTGKBN   =   "1" )
                       COMPUTE CNT-HKNTG
                           =   CNT-HKNTG       +   1
                   END-IF
               END-IF
      *
      *        患者病名チェック
               IF    ( FLG-CHKEND (5)  =   ZERO )
                   PERFORM 2001121-PTBYO-CHK-SEC
               END-IF
      *
      *        疑い病名チェック
               IF    ( FLG-CHKEND (7)  =   ZERO )
                   PERFORM 2001121-PTBYO-UTAGAI-CHK-SEC
               END-IF
      *
      *        急性病名チェック
               IF    ( FLG-CHKEND (7)  =   ZERO )
                   PERFORM 2001121-PTBYO-KYUSEI-CHK-SEC
               END-IF
      *
           END-IF
      *
           .
       2001121-PTBYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者病名チェック付加情報編集処理
      *****************************************************************
       2001121-TPTBYOEX-EDIT-SEC       SECTION.
      *
           INITIALIZE                  TPTBYOEX-AREA
      *
           IF    ( PTBYO-BYOMEICD (1)  =   "0000999" )
            OR   ( PTBYO-BYOMEIHENFLG  =   "1" )
               CONTINUE
           ELSE
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >       CONST-MKCD-MAX )
      *
      *            病名のの廃止年月日が診療開始日よりも前かチェック
                   IF    ( PTBYO-BYOMEICD(IDX0)    NOT =   SPACE )
      *
                       IF    ( PTBYO-BYOMEICD(IDX0) (1:3)  NOT = "ZZZ" )
                           MOVE    PTBYO-BYOMEICD(IDX0)
                                           TO  WRK-BYOMEICD
                           PERFORM 900-BYOMEI-KEY-SEL-SEC
                           IF    ( BYO-HAISIYMD    <   PTBYO-SRYYMD )
                               MOVE    "1"     TO  TPTBYOEX-HAISHIKBN
                           END-IF
      *
                           MOVE    BYO-HKNSKYKBN
                                               TO  TPTBYOEX-HKNTGKBN
      *
                       ELSE
                           COMPUTE TPTBYOEX-SHUSHOKUGO-CNT
                               =   TPTBYOEX-SHUSHOKUGO-CNT +   1
                       END-IF
                   END-IF
               END-PERFORM
      *
               IF    ( TPTBYOEX-SHUSHOKUGO-CNT >   ZERO )
                   MOVE    ZERO                TO  TPTBYOEX-HKNTGKBN
               END-IF
      *
           END-IF
      *
           .
       2001121-TPTBYOEX-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者病名エラーメッセージ付加情報編集
      *****************************************************************
       2001121-PTBYOMES-PLUS-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-PTBYO-MOJI
           INITIALIZE                      WRK-PTBYOMES-PLUS
      *
           IF    ( WRK-PTBYOMEI        =   SPACE )
               MOVE    1               TO  FLG-PTBYO-MOJI
           ELSE
               MOVE    WRK-PTBYOMEI    TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF    ( FLG-KANACHK NOT =   ZERO )
                   MOVE    2           TO  FLG-PTBYO-MOJI
               END-IF
           END-IF
      *
           IF    ( FLG-PTBYO-MOJI      =   ZERO )
               MOVE    WRK-PTBYOMEI    TO  WRK-PTBYOMES-PLUS
           ELSE
      *
      *        病名が不正の場合、診療開始日を編集
               MOVE    WRK-PTBYOSRYYMD TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-EDTYMD1     TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
      *
               STRING  "診療開始日："      DELIMITED   BY  SIZE
                       WRK-OUT-INPUT       DELIMITED   BY  SPACE
               INTO    WRK-PTBYOMES-PLUS
               END-STRING
           END-IF
      *
           .
       2001121-PTBYOMES-PLUS-EXT.
           EXIT.
      *****************************************************************
      *    患者病名チェック
      *****************************************************************
       2001121-PTBYO-CHK-SEC           SECTION.
      *
           IF    ( FLG-PTBYO-MOJI      =   1 )
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               MOVE    "050004"            TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (5)
                   GO  TO  2001121-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
           IF    ( FLG-PTBYO-MOJI      =   2)
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               MOVE    "050005"            TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (5)
                   GO  TO  2001121-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
           IF    ( PTBYO-BYOMEIMOJI    =   ZERO )
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               MOVE    "050006"            TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (5)
                   GO  TO  2001121-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
           IF    ( PTBYO-BYOMEI        NOT = SPACE)  
            AND  ( PTBYO-BYOMEICD-G        = SPACE)
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               MOVE    "050007"            TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (5)
                   GO  TO  2001121-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
           IF   (( PTBYO-TENKIKBN      =   SPACE)
             AND ( PTBYO-TENKIYMD  NOT =   SPACE))
            OR  (( PTBYO-TENKIKBN  NOT =   SPACE)
             AND ( PTBYO-TENKIYMD      =   SPACE))
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               MOVE    "050008"            TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (5)
                   GO  TO  2001121-PTBYO-CHK-EXT
               END-IF
           END-IF
      *
           IF    ( PTBYO-CHARTBYOMEI   NOT =   SPACE )
               MOVE    PTBYO-CHARTBYOMEI   TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF    ( FLG-KANACHK     NOT =   ZERO )
                   MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
                   MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
                   MOVE    "050009"            TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 213-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (5)
                       GO  TO  2001121-PTBYO-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-RECEDEN-CHK     =   1 )
      *
               MOVE    ZERO            TO  FLG-PTBYO-LEN-ERR
      *
               IF    ( PTBYO-BYOMEICD (1)  =   "0000999" )
                OR   ( PTBYO-BYOMEIHENFLG  =   "1" )
      *
                   PERFORM 2001121-PTBYO-STRLEN-CHK-SEC
               ELSE
                   IF    ( TPTBYOEX-HAISHIKBN  =   "1" )
                       PERFORM 2001121-PTBYO-STRLEN-CHK-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-PTBYO-LEN-ERR   NOT =   ZERO )
                   IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                       MOVE    1       TO  FLG-CHKEND (5)
                       GO  TO  2001121-PTBYO-CHK-EXT
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( PTBYO-TENKIKBN  =   SPACE )
               PERFORM 2001121-PTBYO-BYOMEI-HEN-SEC
               IF    ( SYS-1101-BYOSRYKAKBN    =   "1" OR SPACE )
                   MOVE    PTBYO-SRYKA     TO  WRK-DTCHK009-SRYKA
               ELSE
                   MOVE    "00"            TO  WRK-DTCHK009-SRYKA
               END-IF
      *
               INITIALIZE  DTCHK009-REC
               MOVE    WRK-DTCHK009-SRYKA  TO  DTCHK009-SRYKA
               MOVE    WRK-HEN-PTBYOMEI    TO  DTCHK009-BYOMEI
               MOVE    PTBYO-HOSOKU-COMMENT TO DTCHK009-HOSOKU
               PERFORM 900-DTCHK009-READ-SEC
               IF    ( FLG-DTCHK009    =   ZERO )
                   MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
                   MOVE    WRK-HEN-PTBYOMEI2   TO  WRK-ERRMES-PLUS
                   MOVE    "050012"            TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 213-ERRMES-HEN-SEC
               ELSE
                   MOVE    WRK-DTCHK009-SRYKA  TO  DTCHK009-SRYKA
                   MOVE    WRK-HEN-PTBYOMEI    TO  DTCHK009-BYOMEI
                   MOVE    PTBYO-HOSOKU-COMMENT TO DTCHK009-HOSOKU
                   WRITE   DTCHK009-REC
               END-IF
           END-IF
      *
           IF    ( PTINF-BIRTHDAY          >   PTBYO-SRYYMD )
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               MOVE    "050014"            TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 213-ERRMES-HEN-SEC
           END-IF
      *
           .
       2001121-PTBYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者病名対象チェック
      *****************************************************************
       2001121-PTBYO-TAISYO-CHK-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-PTBYO-TAISYO
      *
           IF    ( PTBYO-SRYYMD (1:6)      <=   ORCBSD01-SRYYM )
               IF    ( PTBYO-TENKIKBN       =   SPACE )
                   IF    ( PTBYO-NYUGAIKBN  =   SPACE )
                    OR   ( PTBYO-NYUGAIKBN  =   ORCBSD01-NYUGAIKBN )
                           MOVE    1       TO  FLG-PTBYO-TAISYO
                   END-IF
               ELSE
                   IF    ( PTBYO-TENKIYMD (1:6)
                                           >=   ORCBSD01-SRYYM )
                       IF    ( PTBYO-NYUGAIKBN  =   SPACE )
                        OR   ( PTBYO-NYUGAIKBN  =   ORCBSD01-NYUGAIKBN )
                           MOVE    1       TO  FLG-PTBYO-TAISYO
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       2001121-PTBYO-TAISYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名文字列長チェック処理
      *****************************************************************
       2001121-PTBYO-STRLEN-CHK-SEC    SECTION.
      *
      *    病名編集処理（疑いフラグを考慮して文字数を求める）
           PERFORM 2001121-PTBYO-BYOMEI-HEN-SEC
      *
           IF    ( CNT-BYOMEIMOJI      >   40 )
               MOVE    2           TO  FLG-PTBYO-LEN-ERR
           END-IF
      *
           IF    ( FLG-PTBYO-LEN-ERR       NOT =   ZERO )
      *
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
      *
      *        病名に半角文字が混在していない場合、
      *        病名の先頭５文字まで編集
               MOVE    WRK-HEN-PTBYOMEI
                                       TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF  ( FLG-KANACHK       =  ZERO  )
                   MOVE    SPACE               TO  WRK-ERRMES-PLUS
                   STRING  WRK-HEN-PTBYOMEI (1:10)
                                               DELIMITED   BY  SIZE
                           "〜"                DELIMITED   BY  SIZE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
               ELSE
                   MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               END-IF
      *
               IF    ( FLG-PTBYO-LEN-ERR   =   1 )
                   MOVE    "050010"        TO  WRK-ERRCD
               ELSE
                   MOVE    "050011"        TO  WRK-ERRCD
               END-IF
      *
               INITIALIZE              TERRINF-AREA
               PERFORM 213-ERRMES-HEN-SEC
      *
           END-IF
      *
           .
      *
       2001-PTBYO-STRLEN-CHK-EXT.
           EXIT.
           .
      *
       2001121-MICODKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       2001121-PTBYO-BYOMEI-HEN-SEC       SECTION.
      *
           INITIALIZE                  ORCSMKBYOMEIAREA
           MOVE    PTBYO-UTAGAIFLG TO  SMKBYO-UTAGAIFLG
           MOVE    PTBYO-BYOMEI    TO  SMKBYO-BYOMEI
           CALL    "ORCSMKBYOMEI"  USING   ORCSMKBYOMEIAREA
      *
           MOVE    SMKBYO-BYOMEI   TO  WRK-HEN-PTBYOMEI
           MOVE    SMKBYO-LENGTH   TO  CNT-BYOMEIMOJI
      *
           INITIALIZE                  ORCSMKBYOMEIAREA
           MOVE    PTBYO-UTAGAIFLG TO  SMKBYO-UTAGAIFLG
           MOVE    PTBYO-BYOMEI    TO  SMKBYO-BYOMEI
           MOVE    PTBYO-HOSOKU-COMMENT
                                   TO  SMKBYO-HOSOKU-COMMENT
           CALL    "ORCSMKBYOMEI"  USING   ORCSMKBYOMEIAREA
           MOVE    SMKBYO-BYOMEI   TO  WRK-HEN-PTBYOMEI2
      *
           .
       2001121-PTBYO-BYOMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療開始日チェック（１）処理
      *****************************************************************
       2001121-PTBYO-SRYYMD-CHK-SEC    SECTION.
      *
           MOVE    SPACE   TO  WRK-SYOSIN-CHK-YMD
      *
           IF    ( SYS-1101-BYOSRYKAKBN    =   "1" OR SPACE )
               MOVE    TSRYKA-SYOSIN-YMD(IDX4)
                               TO  WRK-SYOSIN-CHK-YMD
           ELSE
               PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL ( IDX4    >   TSRYKA-MAX )
                   IF    ( WRK-SYOSIN-CHK-YMD  =   SPACE )
                       MOVE    TSRYKA-SYOSIN-YMD(IDX4)
                               TO  WRK-SYOSIN-CHK-YMD
                   ELSE
                       IF    ( TSRYKA-SYOSIN-YMD (IDX4)
                                   NOT =   SPACE )
                        AND  ( TSRYKA-SYOSIN-YMD (IDX4)
                                       >   WRK-SYOSIN-CHK-YMD )
                           MOVE    TSRYKA-SYOSIN-YMD(IDX4)
                               TO  WRK-SYOSIN-CHK-YMD
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    ZERO                TO  FLG-PTBYO-ERR
      *
           IF    ( WRK-SYOSIN-CHK-YMD  NOT =   SPACE )
            AND  ( PTBYO-SRYYMD            <   WRK-SYOSIN-CHK-YMD )
               IF    ( PTBYO-TENKIKBN      =   SPACE )
                           MOVE    1   TO  FLG-PTBYO-ERR
               ELSE
                   EVALUATE    TRUE
                   WHEN   ( PTBYO-TENKIYMD >   WRK-SYOSIN-CHK-YMD )
                           MOVE    1   TO  FLG-PTBYO-ERR
                   WHEN   ( PTBYO-TENKIYMD =   WRK-SYOSIN-CHK-YMD )
                           MOVE    2   TO  FLG-PTBYO-ERR
                   END-EVALUATE
               END-IF
           END-IF
      *
           IF    ( FLG-PTBYO-ERR   NOT =   ZERO )
               MOVE    WRK-SYOSIN-CHK-DD   TO  WRK-DAY-Z
               MOVE    WRK-DAY-Z           TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               INITIALIZE  WRK-ERRMES-PLUS
               STRING  WRK-PTBYOMES-PLUS   DELIMITED   BY  SPACE
                       "　"                DELIMITED   BY  SIZE
                       WRK-OUT-INPUT       DELIMITED   BY  SPACE
                       "日"                DELIMITED   BY  SIZE
               INTO    WRK-ERRMES-PLUS
               END-STRING
               EVALUATE    FLG-PTBYO-ERR
               WHEN    1
                   MOVE    "060002"        TO  WRK-ERRCD
               WHEN    2
                   MOVE    "060004"        TO  WRK-ERRCD
               END-EVALUATE
               INITIALIZE              TERRINF-AREA
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (6) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (6)
               END-IF
           END-IF
      *
           .
      *
       2001121-PTBYO-SRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    疑い病名チェック処理
      *****************************************************************
       2001121-PTBYO-UTAGAI-CHK-SEC    SECTION.
      *
           IF    ( PTBYO-UTAGAIFLG     = "1" OR "3")
            AND  ( PTBYO-TENKIKBN      = SPACE)
            AND  ( PTBYO-SRYYMD (1:6) <= WRK-UTAGAI-CHK-YM )
               MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
               MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
               MOVE    "070001"            TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (7) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (7)
                   GO  TO  2001121-PTBYO-UTAGAI-CHK-EXT
               END-IF
           END-IF
      *
           .
       2001121-PTBYO-UTAGAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    急性病名チェック処理
      *****************************************************************
       2001121-PTBYO-KYUSEI-CHK-SEC    SECTION.
      *
           IF    ( WRK-KYUSEI-CHK-YM   NOT =   SPACE )
      *
               IF    ( PTBYO-UTAGAIFLG     = "2" OR "3")
                AND  ( PTBYO-TENKIKBN      = SPACE)
                AND  ( PTBYO-SRYYMD (1:6) <= WRK-KYUSEI-CHK-YM )
                   MOVE    PTBYO-SRYKA         TO  SRH-1005-SRYKA
                   MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
                   MOVE    "070002"            TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 213-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (7) =   "1" )
                       MOVE    1           TO  FLG-CHKEND (7)
                       GO  TO  2001121-PTBYO-KYUSEI-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           .
       2001121-PTBYO-KYUSEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名診療開始日編集処理
      *****************************************************************
       2001121-PTBYO-DAY-HEN-SEC       SECTION.
      *
           MOVE    "99999999"          TO  WRK-TENKIYMD
      *
           IF    ( PTBYO-TENKIYMD      NOT =   SPACE )
               MOVE    PTBYO-TENKIYMD  TO  WRK-TENKIYMD
           END-IF
      *
           IF    ( PTBYO-SRYYMD (1:6)  =   ORCBSD01-SRYYM )
               MOVE    PTBYO-SRYYMD    TO  WRK-YMD
               IF    ( WRK-DD          >=  1 )
                AND  ( WRK-DD          <=  31 )
                   IF    ( SYS-1101-BYOSRYKAKBN    =   "1" OR SPACE )
                       MOVE    1       TO  FLG-PTBYO-DAY-KA(IDX4 WRK-DD)
                   ELSE
                       MOVE    1       TO  FLG-PTBYO-DAY   (WRK-DD)
                   END-IF
               END-IF
           END-IF
      *
           MOVE    ORCBSD01-SRYYM      TO  WRK-YMD
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    IDXD            TO  WRK-DD
               IF    ( WRK-YMD         >=  PTBYO-SRYYMD )
                AND  ( WRK-YMD         <=  WRK-TENKIYMD )
                   IF    ( SYS-1101-BYOSRYKAKBN    =   "1" OR SPACE )
                       MOVE    1   TO  FLG-PTBYO-DAY2-KA (IDX4 WRK-DD)
                   ELSE
                       MOVE    1   TO  FLG-PTBYO-DAY2    (WRK-DD)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       2001121-PTBYO-DAY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    再診料算定日設定処理
      *****************************************************************
       2001121-SAISIN-DAY-HEN-SEC      SECTION.
      *
           MOVE    "99999999"          TO  WRK-TENKIYMD
      *
           IF    ( PTBYO-TENKIYMD      NOT =   SPACE )
               IF    ( PTBYO-TENKIKBN  =   "3" )
                AND  ( SYS-1101-SRYSTTENKIKBN  =   "1" )
                   MOVE    PTBYO-TENKIYMD  TO  WRK-SYMD
                   MOVE    "2"             TO  WRK-ZOGENPTN
                   MOVE     1              TO  WRK-ZOGEN
                   PERFORM 800-CALENDAR-SEC
                   MOVE    "1"             TO  WRK-ZOGENPTN
                   MOVE     -1             TO  WRK-ZOGEN
                   PERFORM 800-CALENDAR-SEC
                   MOVE    WRK-SYMD        TO  WRK-TENKIYMD
               ELSE
                   MOVE    PTBYO-TENKIYMD  TO  WRK-TENKIYMD
               END-IF
           END-IF
      *
           MOVE    TSRYKA-SYOSAIYMD-MAX (IDX4) TO  WRK-MAX
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   WRK-MAX )
      *
               IF    ( PTBYO-SRYYMD    <  TSRYKA-SYOSAIYMD-SAISINYMD
                                                         (IDX4 IDX0))
                AND  ( WRK-TENKIYMD    >= TSRYKA-SYOSAIYMD-SYOSINYMD
                                                            (IDX4 IDX0))
                   IF    ( WRK-TENKIYMD
                                       <  TSRYKA-SYOSAIYMD-SAISINYMD
                                                            (IDX4 IDX0))
                       IF    ( PTBYO-TENKIKBN  =   "1"  OR "3" )
                           MOVE  1  TO TSRYKA-TIYU-CHUSHI(IDX4 IDX0)
                       END-IF
                   ELSE
                           MOVE  1  TO TSRYKA-BYO-KEIZOKU(IDX4 IDX0)
                   END-IF
               END-IF
      *
               IF    ( PTBYO-SRYYMD =   TSRYKA-SYOSAIYMD-SAISINYMD
                                                            (IDX4 IDX0))
                AND  ( FLG-TAISYO-BYOMEI       =   1 )
                   IF    ( SYS-1101-BYOSRYKAKBN
                                               =   "1" OR SPACE )
                       IF    ( PTBYO-SRYKA     =   TSRYKA-SRYKA  (IDX4))
                           MOVE  1  TO TSRYKA-BYO-KAISHI (IDX4 IDX0)
                       END-IF
                   ELSE
                           MOVE  1  TO TSRYKA-BYO-KAISHI (IDX4 IDX0)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       2001121-SAISIN-DAY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者病名登録チェック
      *****************************************************************
       2001121-PTBYO-TOUROKU-CHK-SEC    SECTION.
      *
           IF    ( CNT-PTBYOMEI1   =   ZERO )
               MOVE    WRK-SRYKA   TO  SRH-1005-SRYKA
               MOVE    "050001"    TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 214-ERRMES-HEN-SEC
      *
           ELSE
               IF    ( CNT-PTBYOMEI2   =   ZERO )
                   MOVE    WRK-SRYKA       TO  SRH-1005-SRYKA
                   IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
                       MOVE    "050002"    TO  WRK-ERRCD
                   ELSE
                       MOVE    "050003"    TO  WRK-ERRCD
                   END-IF
                   INITIALIZE              TERRINF-AREA
                   PERFORM 214-ERRMES-HEN-SEC
      *
               ELSE
                   IF    ( CNT-PTBYOMEI2   =   CNT-HKNTG )
                       MOVE    WRK-SRYKA   TO  SRH-1005-SRYKA
                       MOVE    "050015"    TO  WRK-ERRCD
                       INITIALIZE              TERRINF-AREA
                       PERFORM 214-ERRMES-HEN-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-CHKEND (5) =   ZERO )
               IF    ( SYS-1101-SYUBYOCHKKBN   =   "1" )
                AND  ( CNT-PTBYOMEI1  >   ZERO )
                AND  ( CNT-PTBYOMEI2  >   ZERO )
                AND  ( CNT-PTSYUBYO   =   ZERO )
                   MOVE    WRK-SRYKA       TO  SRH-1005-SRYKA
                   MOVE    "050013"        TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 214-ERRMES-HEN-SEC
      *
                   IF    ( ORCBSD01-CHKKBN (5) =   "1" )
                       MOVE    1       TO  FLG-CHKEND (5)
                       GO  TO  2001121-PTBYO-TOUROKU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           .
       2001121-PTBYO-TOUROKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初再診期間編集処理
      *****************************************************************
       200112-SYOSAISIN-KKN-SEC        SECTION.
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               IF    ( TSRYKA-SAISIN-DAY (IDX4 IDX-DAY)
                                           =   1   )
      *
                   COMPUTE TSRYKA-SYOSAIYMD-MAX (IDX4)
                       =   TSRYKA-SYOSAIYMD-MAX (IDX4)  +   1
      *
                   MOVE    TSRYKA-SYOSAIYMD-MAX (IDX4)
                                   TO  WRK-MAX
      *
                   MOVE    ORCBSD01-SRYYM
                                   TO  TSRYKA-SYOSAIYMD-SAISIN-YM
                                            (IDX4  WRK-MAX)
      *
                   MOVE    IDX-DAY TO  TSRYKA-SYOSAIYMD-SAISIN-DD
                                            (IDX4  WRK-MAX)
      *
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   WRK-MAX )
                            OR   ( TSRYKA-SYOSAIYMD-SYOSINYMD
                                            (IDX4  WRK-MAX)
                                       NOT =   SPACE )
      *
                       IF    ( WRK-SYOSINYMD (IDX0)
                                      <=   TSRYKA-SYOSAIYMD-SAISINYMD
                                            (IDX4  WRK-MAX))
                           MOVE    WRK-SYOSINYMD (IDX0)
                                       TO  TSRYKA-SYOSAIYMD-SYOSINYMD
                                            (IDX4  WRK-MAX)
                       END-IF
      *
                   END-PERFORM
      *
                   IF    ( TSRYKA-SYOSAIYMD-SYOSINYMD
                                            (IDX4  WRK-MAX)
                                       =   SPACE )
                       MOVE    ZERO    TO  TSRYKA-SYOSAIYMD-SYOSINYMD
                                            (IDX4  WRK-MAX)
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200112-SYOSAISIN-KKN-EXT.
           EXIT.
      *****************************************************************
      *    診療開始日チェック（２）処理
      *****************************************************************
       200112-PTBYO-SRYYMD-CHK-SEC     SECTION.
      *
           INITIALIZE              FLG-SYOSIN-ERR-DAY-G
                                   FLG-SAISIN-ERR-DAY-G
                                   CNT-JNISSU
                                   CNT-SRYYMD
                                   WRK-JNISSU-DAY-G
      *
           IF    ( SYS-1101-BYOSRYKAKBN    =   "1" OR SPACE )
               MOVE    FLG-PTBYO-DAY-KA-OCC (IDX4)
                                      TO   FLG-PTBYO-DAY-G
               MOVE    FLG-PTBYO-DAY2-KA-OCC (IDX4)
                                      TO   FLG-PTBYO-DAY2-G
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0           >   31 )
      *
               IF    ( TSRYKA-SYOSIN-DAY (IDX4 IDX0)   =   1 )
                AND  ( FLG-PTBYO-DAY (IDX0)            =   0 )
                   MOVE    1          TO   FLG-SYOSIN-ERR-DAY (IDX0)
               END-IF
      *
               IF    ( SYS-1101-BYOSRYKAKBN    =   "1" OR SPACE )
                   IF    ( TSRYKA-JNISSU-DAY   (IDX4 IDX0) =   1 )
                    AND  ( TSRYKA-SYOSIN-DAY (IDX4 IDX0)   =   0 )
                    AND  ( FLG-PTBYO-DAY2 (IDX0)           =   0 )
                       MOVE    1      TO   FLG-SAISIN-ERR-DAY (IDX0)
                   END-IF
                   IF    ( TSRYKA-JNISSU-DAY   (IDX4 IDX0) =   1 )
                       COMPUTE CNT-JNISSU  =   CNT-JNISSU  +   1
                       MOVE    1      TO   WRK-JNISSU-DAY (IDX0)
                   END-IF
               ELSE
                   IF    ( TSRYKA-JNISSU-DAY   (IDX4 IDX0) =   1 )
                    AND  ( TSRYKA-ALL-SYOSIN-DAY (IDX0)    =   0 )
                    AND  ( FLG-PTBYO-DAY2 (IDX0)           =   0 )
                       MOVE    1      TO   FLG-SAISIN-ERR-DAY (IDX0)
                   END-IF
                   IF    ( TSRYKA-ALL-JNISSU-DAY  (IDX0)   =   1 )
                       COMPUTE CNT-JNISSU  =   CNT-JNISSU  +   1
                       MOVE    1      TO   WRK-JNISSU-DAY (IDX0)
                   END-IF
               END-IF
      *
               IF    ( FLG-PTBYO-DAY (IDX0)    =   1 )
                   COMPUTE CNT-SRYYMD      =   CNT-SRYYMD  +   1
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-SYOSIN-ERR-DAY-G    NOT =   ZERO )
               MOVE    FLG-SYOSIN-ERR-DAY-G
                                       TO  WRK-NISSU-DAY-G
               PERFORM 200111-NISSU-HEN-SEC
               MOVE    TSRYKA-SRYKA (IDX4)
                                       TO  SRH-1005-SRYKA
               MOVE    WRK-NISSU-MES   TO  WRK-ERRMES-PLUS
               MOVE    "060001"        TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 213-ERRMES-HEN-SEC
           END-IF
      *
           IF    ( FLG-SAISIN-ERR-DAY-G    NOT =   ZERO )
               MOVE    FLG-SAISIN-ERR-DAY-G
                                       TO  WRK-NISSU-DAY-G
               PERFORM 200111-NISSU-HEN-SEC
               MOVE    TSRYKA-SRYKA (IDX4)
                                       TO  SRH-1005-SRYKA
               MOVE    WRK-NISSU-MES   TO  WRK-ERRMES-PLUS
               MOVE    "060007"        TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 213-ERRMES-HEN-SEC
           END-IF
      *
           IF    ( CNT-SRYYMD          >   CNT-JNISSU )
               IF    ( SYS-1101-COMPBYONISSUKBN  =   "1" )
      *
                   MOVE    SPACE           TO  WRK-ERRMES-PLUS
                   MOVE    FLG-PTBYO-DAY-G TO  WRK-NISSU-DAY-G
                   PERFORM 200111-NISSU-HEN-SEC
                   MOVE    WRK-NISSU-MES   TO  WRK-STR
                   MOVE    WRK-JNISSU-DAY-G
                                           TO  WRK-NISSU-DAY-G
                   PERFORM 200111-NISSU-HEN-SEC
                   STRING  "診療開始日"
                                                   DELIMITED BY SIZE
                           WRK-STR                 DELIMITED BY SPACE
                           "／診療日"              DELIMITED BY SIZE
                           WRK-NISSU-MES           DELIMITED BY SPACE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
      *
                   IF    ( SYS-1101-BYOSRYKAKBN    =   "1" OR SPACE )
                       MOVE    TSRYKA-SRYKA (IDX4)
                                           TO  SRH-1005-SRYKA
                   ELSE
                       MOVE    LOW-VALUE   TO  SRH-1005-SRYKA
                   END-IF
      *
                   MOVE    "060006"    TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 213-ERRMES-HEN-SEC
               END-IF
           END-IF
      *
           .
       200112-PTBYO-SRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療開始日チェック（３）処理
      *****************************************************************
       200112-PTBYO-SAISIN-CHK-SEC     SECTION.
      *
           INITIALIZE              FLG-SAISIN-ERR-DAY-G
      *
           MOVE    TSRYKA-SYOSAIYMD-MAX (IDX4) TO  WRK-MAX
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   WRK-MAX )
      *
               IF    ( TSRYKA-SYOSAIYMD-SAISINYMD (IDX4 IDX0)
                                   NOT =   ZERO )
                AND  ( TSRYKA-TIYU-CHUSHI (IDX4 IDX0)
                                       =   WRK-TIYU-CHUSHI )
                AND  ( TSRYKA-BYO-KAISHI  (IDX4 IDX0)  =   1 )
                AND  ( TSRYKA-BYO-KEIZOKU (IDX4 IDX0)  =   ZERO )
                   MOVE    TSRYKA-SYOSAIYMD-SAISINYMD (IDX4 IDX0)
                                   TO  WRK-YMD
                   IF    ( WRK-DD          >=  1 )
                    AND  ( WRK-DD          <=  31 )
                       MOVE    1      TO  FLG-SAISIN-ERR-DAY (WRK-DD)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-SAISIN-ERR-DAY-G  NOT = ZERO )
      *
               INITIALIZE  WRK-NISSU-AREA
               MOVE    FLG-SAISIN-ERR-DAY-G
                                   TO  WRK-NISSU-DAY-G
      *
               PERFORM 200111-NISSU-HEN-SEC
      *
               MOVE    TSRYKA-SRYKA (IDX4)
                                   TO  SRH-1005-SRYKA
               MOVE    WRK-NISSU-MES
                                   TO  WRK-ERRMES-PLUS
               MOVE    WRK-SAISHIN-ERRCD
                                   TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (6) =   "1" )
                   MOVE    1       TO  FLG-CHKEND (6)
               END-IF
           END-IF
      *
           .
       200112-PTBYO-SAISIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診算定日チェック処理
      *****************************************************************
       2001121-SYOSIN-SANTEI-SEC       SECTION.
      *
           INITIALIZE                  WRK-MAE-SRYYM-G
                                       WRK-KON-SRYYM-G
      *
           PERFORM 900-SANTEI-KEY11-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SANTEI     NOT  =   ZERO )
      *
               PERFORM VARYING IDX-DAY    FROM     1   BY  1
                       UNTIL ( IDX-DAY    >    31 )
                        OR   ( FLG-SANTEI NOT  =   ZERO )
      *
                   IF    ( SANTEI-DAY (IDX-DAY)    >   ZERO )
                       IF    ( SANTEI-SRYYM    =   ORCBSD01-SRYYM )
                           MOVE    SANTEI-SRYYM    TO  WRK-KON-SRYYM
                           MOVE    1       TO  WRK-KON-SRYYM-DAY
                                                            (IDX-DAY)
                       ELSE
                           IF    ( WRK-MAE-SRYYM   =   SPACE )
                               MOVE    SANTEI-SRYYM
                                           TO  WRK-MAE-SRYYM
                               MOVE    1   TO  WRK-MAE-SRYYM-DAY
                                                            (IDX-DAY)
                           ELSE
                               IF    ( SANTEI-SRYYM    = WRK-MAE-SRYYM )
                                   MOVE    1   TO  WRK-MAE-SRYYM-DAY
                                                            (IDX-DAY)
                               ELSE
                                   MOVE    1   TO  FLG-SANTEI
                               END-IF
                           END-IF
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               IF    ( FLG-SANTEI      =   ZERO )
                   PERFORM 900-SANTEI-KEY11-FET-SEC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_santei"    TO  MCP-TABLE
           MOVE    "key11"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( WRK-KON-SRYYM   NOT =   SPACE )
               PERFORM VARYING IDX-DAY    FROM     31  BY  -1
                       UNTIL ( IDX-DAY    <    1 )
      *
                   IF ( WRK-KON-SRYYM-DAY (IDX-DAY)
                                       =  1 )
                       COMPUTE WRK-SYOSINYMD-MAX
                           =   WRK-SYOSINYMD-MAX   +   1
                       MOVE    WRK-KON-SRYYM   TO  WRK-SYOSIN-YM
                                                    (WRK-SYOSINYMD-MAX)
                       MOVE    IDX-DAY         TO  WRK-SYOSIN-DD
                                                    (WRK-SYOSINYMD-MAX)
                   END-IF
               END-PERFORM
           END-IF
      *
           IF    ( WRK-MAE-SRYYM   NOT =   SPACE )
               PERFORM VARYING IDX-DAY     FROM     31  BY  -1
                       UNTIL ( IDX-DAY     <    1 )
      *
                   IF ( WRK-MAE-SRYYM-DAY (IDX-DAY)
                                       =  1 )
                       COMPUTE WRK-SYOSINYMD-MAX
                           =   WRK-SYOSINYMD-MAX   +   1
                       MOVE    WRK-MAE-SRYYM   TO  WRK-SYOSIN-YM
                                                    (WRK-SYOSINYMD-MAX)
                       MOVE    IDX-DAY         TO  WRK-SYOSIN-DD
                                                    (WRK-SYOSINYMD-MAX)
                       MOVE    1               TO  IDX-DAY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       2001121-SYOSIN-SANTEI-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名チェック処理
      *****************************************************************
       200112-YKZ-BYO-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >       TSRYKA-MAX )
                     OR  ( FLG-CHKEND (8)      =   1 )
      *
               PERFORM VARYING IDX7    FROM    1   BY  1
                       UNTIL ( IDX7    >   TSRYKA-SRYCD-MAX (IDX6))
                         OR  ( FLG-CHKEND (8)  =   1 )
      *
                   IF    ( TSRYKA-SRYCD (IDX6 IDX7)(1:1)
                                           =   "6" OR "S" )
                       MOVE    "08"    TO  WRK-REIGAI-CHKNUM
                       MOVE    "1"     TO  WRK-REIGAI-CHKKBN
                       MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-REIGAI-CD
                       PERFORM 800-REIGAI-CHK-SEC
                       IF    ( FLG-REIGAI  =   ZERO )
                           MOVE    "1"     TO  WRK-CHK-CHKKBN
                           MOVE    "1"     TO  WRK-CHK-CDKBN
                           MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                           TO  WRK-CHK-SRYCD
                           PERFORM 900-DTCHK008-READ-SEC
                           IF    ( FLG-DTCHK008    =   1 )
                               PERFORM 200112-CHKMST-YKZ-BYO-CHK-SEC
                           END-IF
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           .
       200112-YKZ-BYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名チェック処理（チェックマスタチェック）
      *****************************************************************
       200112-CHKMST-YKZ-BYO-CHK-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           MOVE    "1"                 TO  WRK-CHK-CHKKBN
           MOVE    "M"                 TO  WRK-CHK-CDKBN
           MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-CHK-SRYCD
           PERFORM 900-DTCHK008-READ-SEC
           IF    ( FLG-DTCHK008        =   ZERO )
               CONTINUE
           ELSE
               PERFORM 900-DTCHK008-WRITE-SEC
      *
               MOVE    "1"             TO  WRK-CHK-CHKKBN
               MOVE    "1"             TO  WRK-CHK-CDKBN
               MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-CHK-SRYCD
               PERFORM 800-TMPSND-EDIT-SEC
           END-IF
      *
           INITIALIZE                      CHKSND-REC
           MOVE    "1"                 TO  CHKSND-CHKKBN
           MOVE    "0"                 TO  CHKSND-CDKBN
           MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  CHKSND-SRYCD
           MOVE    WRK-KJNYMD          TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM    TO  CHKSND-HOSPNUM
           PERFORM 901-CHKSND-KEY3-SEL-SEC
           IF    ( FLG-CHKSND          =   ZERO )
               MOVE    CHKSND-REC      TO  TCHKSND-REC
               IF    ( CHKSND-NYUGAIKBN    =   SPACE )
                OR   ( CHKSND-NYUGAIKBN    =   ORCBSD01-NYUGAIKBN )
                   CONTINUE
               ELSE
                   MOVE    2           TO  FLG-CHKMST
               END-IF
           END-IF
      *
      *    CDKBN="1"のデータが存在する場合はチェックを行う
           INITIALIZE                      CHKSND-REC
           MOVE    "1"                 TO  CHKSND-CHKKBN
           MOVE    "1"                 TO  CHKSND-CDKBN
           MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  CHKSND-SRYCD
           MOVE    WRK-KJNYMD          TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM    TO  CHKSND-HOSPNUM
           PERFORM 901-CHKSND-KEY3-SEL-SEC
           IF    ( FLG-CHKSND          =   ZERO )
               CONTINUE
           ELSE
               IF    ( FLG-CHKMST      =   ZERO )
                   MOVE    "1"             TO  WRK-CHK-CHKKBN
                   MOVE    "1"             TO  WRK-CHK-CDKBN
                   MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                           TO  WRK-CHK-SRYCD
                   PERFORM 901-DTCHK008-WRITE-SEC
                   MOVE    1               TO  FLG-CHKMST
               END-IF
           END-IF
      *
           IF    ( FLG-CHKMST          =   ZERO )
      *
               MOVE    CHKSND-REC      TO  TCHKSND-REC
      *
               MOVE    TSRYKA-SRYCD-DAY-G (IDX6 IDX7)
                                       TO  WRK-CHKDAY-G
               MOVE    "1"             TO  WRK-CHK-CHKKBN
               MOVE    "3"             TO  WRK-CHK-CDKBN
               MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-CHK-SRYCD
               PERFORM 900-DTCHK008-READ-SEC
               IF    ( FLG-DTCHK008    =   1 )
                   PERFORM 2001121-REIGAIYKZ-CHK-SEC
               END-IF
               IF    ( WRK-CHKDAY-G    NOT =   ZERO )
      *
                   MOVE    TCHKSND-REC     TO  CHKSND-REC
      *
                   INITIALIZE                  SDTCHK01AREA
                   IF    ( SYS-1101-BYOCHKPTN-G    =   SPACE )
                       MOVE    CONST-CHKPTN    TO  SDTCHK01-CHKPTN-G
                   ELSE
                       MOVE    SYS-1101-BYOCHKPTN-G
                                               TO  SDTCHK01-CHKPTN-G
                   END-IF
                   MOVE    DTCHK003-HOSPNUM TO  SDTCHK01-HOSPNUM
                   MOVE    DTCHK003-PTID   TO  SDTCHK01-PTID
                   MOVE    TSRYKA-SRYKA (IDX6)
                                           TO  SDTCHK01-SRYKA
                   MOVE    CHKSND-CHKKBN   TO  SDTCHK01-CHKKBN
                   MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                           TO  SDTCHK01-SRYCD
                   MOVE    WRK-KJNYMD      TO  SDTCHK01-KJNYMD
                   MOVE    "1"             TO  SDTCHK01-CDKBN
                   MOVE    ORCBSD01-NYUGAIKBN
                                           TO  SDTCHK01-NYUGAIKBN
                   MOVE    DTCHK003-HKNKBN TO  SDTCHK01-HKNKBN
                   MOVE    CHKSND-GRPCHKKBN
                                           TO  SDTCHK01-GRPCHKKBN
                   MOVE    WRK-CHKDAY-G    TO  SDTCHK01-CHKDAY-G
                   CALL    "ORCSDTCHK01"   USING   SDTCHK01AREA
                                                   SYS-1101-REC
                                                   SDTCHK02AREA
                                                   LNK-HKNCOMBI-AREA
                                                   SPA-AREA
                                                   DTCHKCOMMON-AREA
                  IF    ( SDTCHK01-O-OK   =   ZERO )
                       MOVE    TSRYKA-SRYKA (IDX6)
                                           TO  WRK-SRYKA
                       INITIALIZE              TERRINF-AREA
                       PERFORM 200112-YKZ-BYO-ERRMES-SEC
                  END-IF
               END-IF
           ELSE
               MOVE    1               TO  FLG-CHKMST
               MOVE    "1"             TO  WRK-CHK-CHKKBN
               MOVE    "1"             TO  WRK-CHK-CDKBN
               MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-CHK-SRYCD
               PERFORM 900-DTCHK008-WRITE-SEC
           END-IF
      *
           .
       200112-CHKMST-YKZ-BYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名チェック処理（チェックマスタチェック）
      *****************************************************************
       2001121-REIGAIYKZ-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  CNT-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    "1"                 TO  CHKSND-CHKKBN
           MOVE    "3"                 TO  CHKSND-CDKBN
           MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  CHKSND-SRYCD
           MOVE    WRK-KJNYMD          TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM    TO  CHKSND-HOSPNUM
           PERFORM 900-DTCHK010-START-SEC
           IF    ( FLG-DTCHK010        =   ZERO )
               PERFORM UNTIL ( FLG-DTCHK010    NOT =   ZERO )
      *
                   MOVE    DTCHK010-DAT    TO  CHKSND-REC
      *
                   PERFORM 20011211-REIGAIYKZ-CHK-SEC
      *
                   PERFORM 900-DTCHK010-READ-SEC
      *
               END-PERFORM
           ELSE
               PERFORM 900-CHKSND-KEY3-SEL-SEC
      *
               PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
      *
                   PERFORM 20011211-REIGAIYKZ-CHK-SEC
      *
                   MOVE    CHKSND-REC      TO  DTCHK010-DAT
                   WRITE   DTCHK010-REC
      *
                   COMPUTE CNT-CHKSND  =   CNT-CHKSND  +   1
      *
                   PERFORM 900-CHKSND-KEY3-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_chksnd"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               IF    ( CNT-CHKSND      =   ZERO )
                   MOVE    "1"         TO  WRK-CHK-CHKKBN
                   MOVE    "3"         TO  WRK-CHK-CDKBN
                   MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-CHK-SRYCD
                   PERFORM 900-DTCHK008-WRITE-SEC
               END-IF
      *
           END-IF
      *
           .
       2001121-REIGAIYKZ-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名チェック処理（チェックマスタチェック）
      *****************************************************************
       20011211-REIGAIYKZ-CHK-SEC      SECTION.
      *
           IF    ( SYS-1101-BYOSRYKAKBN    =   "1" OR SPACE )
               MOVE    TSRYKA-SRYCD-MAX (IDX6) TO  WRK-MAX
               PERFORM VARYING IDX39   FROM    1   BY  1
                       UNTIL ( IDX39   >   WRK-MAX )
                   IF    ( TSRYKA-SRYCD (IDX6 IDX39)   =   CHKSND-CD )
                       PERFORM VARYING IDX40   FROM    1   BY  1
                               UNTIL ( IDX40   >   31 )
                           IF    ( TSRYKA-SRYCD-DAY (IDX6 IDX39 IDX40)
                                               >   ZERO )
                               MOVE    ZERO    TO  WRK-CHKDAY (IDX40)
                           END-IF
                       END-PERFORM
                   END-IF
               END-PERFORM
           ELSE
               PERFORM VARYING IDX38   FROM    1   BY  1
                       UNTIL ( IDX38   >   TSRYKA-MAX )
                   MOVE    TSRYKA-SRYCD-MAX (IDX38)
                                               TO  WRK-MAX
                   PERFORM VARYING IDX39   FROM    1   BY  1
                           UNTIL ( IDX39   >   WRK-MAX )
                       IF    ( TSRYKA-SRYCD (IDX38 IDX39) = CHKSND-CD )
                           PERFORM VARYING IDX40   FROM    1   BY  1
                                   UNTIL ( IDX40   >   31 )
                               IF  ( TSRYKA-SRYCD-DAY(IDX38 IDX39 IDX40)
                                               >   ZERO )
                                   MOVE    ZERO  TO  WRK-CHKDAY (IDX40)
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           .
       20011211-REIGAIYKZ-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名エラーメッセージ編集処理
      *****************************************************************
       200112-YKZ-BYO-ERRMES-SEC   SECTION.
      *
           IF    ( SDTCHK01-O-NODAY-G      NOT =   ZERO )
      *
               INITIALIZE  WRK-NISSU-AREA
               MOVE    SDTCHK01-O-NODAY-G  TO  WRK-NISSU-DAY-G
      *
               PERFORM 200111-NISSU-HEN-SEC
      *
      *
               MOVE    CHKSND-SRYCD    TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  WRK-SRY-NAME
      *
               IF    ( CHKSND-SRYCD(1:1)   =   "S" OR "P" )
                   MOVE    CHKSND-SRYCD        TO  WRK-SRYCD
                   PERFORM 900-INPUTCD-KEY2-SEL-SEC
                   IF    ( FLG-INPUTCD         =   ZERO )
                       MOVE    ICD-DSPNAME     TO  WRK-SRY-NAME
                   END-IF
               ELSE
      *
                   MOVE    CHKSND-SRYCD        TO  WRK-SRYCD
                   MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU           =   ZERO )
                       MOVE    TNS-NAME        TO  WRK-SRY-NAME
                   END-IF
               END-IF
      *
               MOVE    "080001"            TO  WRK-ERRCD
      *
               MOVE   SPACE                TO  WRK-ERRMES-PLUS
               STRING WRK-SRY-NAME             DELIMITED BY SPACE
                      WRK-NISSU-MES            DELIMITED BY SPACE
               INTO   WRK-ERRMES-PLUS
               END-STRING
      *
               MOVE    WRK-SRYKA           TO  SRH-1005-SRYKA
               INITIALIZE                      TERRINF-AREA
               MOVE    "1"                 TO  TERRINF-CHKKBN
               MOVE    CHKSND-SRYCD        TO  TERRINF-SRYCD
               MOVE    WRK-SRYKA           TO  TERRINF-SRYKA
               MOVE    WRK-NISSU-DAY-G     TO  TERRINF-DAY-G
               MOVE    WRK-NISSU-LASTYMD   TO  TERRINF-LASTYMD
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (8) =   "1" )
                   MOVE    1       TO  FLG-CHKEND (8)
               END-IF
           END-IF
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SDTCHK01-O-MAX )
                        OR   ( FLG-CHKEND (8)  =   1 )
                   INITIALIZE  WRK-NISSU-AREA
      *
                   INITIALIZE  WRK-NISSU-AREA
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
                       IF    ( SDTCHK01-O-ERRDAY (IDX0 IDX-DAY) =   1 )
                           MOVE    1       TO  WRK-NISSU-DAY (IDX-DAY)
                       END-IF
                   END-PERFORM
                   IF    ( WRK-NISSU-DAY-G NOT =   ZERO )
                       MOVE    "080004"   TO  WRK-ERRCD
                       PERFORM 2001121-YKZ-BYO-ERRMES-SEC
                   END-IF
      *
                   INITIALIZE  WRK-NISSU-AREA
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
                       IF    ( SDTCHK01-O-ERRDAY (IDX0 IDX-DAY) =   2 )
                           MOVE    1       TO  WRK-NISSU-DAY (IDX-DAY)
                       END-IF
                   END-PERFORM
                   IF    ( WRK-NISSU-DAY-G NOT =   ZERO )
                       IF    ( ORCBSD01-NYUGAIKBN  =  "1" )
                           MOVE    "080002"   TO  WRK-ERRCD
                       ELSE
                           MOVE    "080003"   TO  WRK-ERRCD
                       END-IF
                       PERFORM 2001121-YKZ-BYO-ERRMES-SEC
                   END-IF
               END-PERFORM
      *
      *
           .
       200112-YKZ-BYO-ERRMES-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と病名エラーメッセージ編集処理
      *****************************************************************
       2001121-YKZ-BYO-ERRMES-SEC   SECTION.
      *
           PERFORM 200111-NISSU-HEN-SEC
      *
           MOVE    CHKSND-SRYCD    TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-SRY-NAME
      *
           IF    ( CHKSND-SRYCD(1:1)   =   "S" OR "P" )
               MOVE    CHKSND-SRYCD        TO  WRK-SRYCD
               PERFORM 900-INPUTCD-KEY2-SEL-SEC
               IF    ( FLG-INPUTCD         =   ZERO )
                   MOVE    ICD-DSPNAME     TO  WRK-SRY-NAME
               END-IF
           ELSE
      *
               MOVE    CHKSND-SRYCD        TO  WRK-SRYCD
               MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   MOVE    TNS-NAME        TO  WRK-SRY-NAME
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  WRK-ERRMES-PLUS
           IF    ( SDTCHK01-O-HOSOKU (IDX0)
                                   =   SPACE )
               STRING  WRK-SRY-NAME        DELIMITED BY SPACE
                       "、"                DELIMITED BY SIZE
                       SDTCHK01-O-BYOMEI (IDX0)
                                           DELIMITED BY SPACE
                       WRK-NISSU-MES       DELIMITED BY SPACE
               INTO    WRK-ERRMES-PLUS
               END-STRING
           ELSE
               STRING  WRK-SRY-NAME        DELIMITED BY SPACE
                       "、"                DELIMITED BY SIZE
                       SDTCHK01-O-BYOMEI (IDX0)
                                           DELIMITED BY SPACE
                       "（"                DELIMITED BY SIZE
                       SDTCHK01-O-HOSOKU (IDX0)
                                           DELIMITED BY SPACE
                       "）"                DELIMITED BY SIZE
                       WRK-NISSU-MES       DELIMITED BY SPACE
               INTO    WRK-ERRMES-PLUS
               END-STRING
           END-IF
      *
           MOVE    WRK-SRYKA       TO  SRH-1005-SRYKA
           INITIALIZE                      TERRINF-AREA
           MOVE    "1"                 TO  TERRINF-CHKKBN
           MOVE    CHKSND-SRYCD        TO  TERRINF-SRYCD
           MOVE    WRK-SRYKA           TO  TERRINF-SRYKA
           MOVE    WRK-NISSU-DAY-G     TO  TERRINF-DAY-G
           MOVE    WRK-NISSU-LASTYMD   TO  TERRINF-LASTYMD
           PERFORM 213-ERRMES-HEN-SEC
      *
           IF    ( ORCBSD01-CHKKBN (8) =   "1" )
               MOVE    1           TO  FLG-CHKEND (8)
           END-IF
      *
           .
       2001121-YKZ-BYO-ERRMES-EXT.
           EXIT.
      *****************************************************************
      *    診療行為と病名チェック処理
      *****************************************************************
       200112-SRY-BYO-CHK-SEC          SECTION.
      *
           PERFORM 2001121-SRY-BYO-CHK-SEC
      *
           PERFORM 2001121-PSET-BYO-CHK-SEC
      *
           .
       200112-SRY-BYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為と病名チェック処理
      *****************************************************************
       2001121-SRY-BYO-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >       TSRYKA-MAX )
                     OR  ( FLG-CHKEND (9)      =   1 )
      *
               PERFORM VARYING IDX7    FROM    1   BY  1
                       UNTIL ( IDX7    >   TSRYKA-SRYCD-MAX (IDX6))
                         OR  ( FLG-CHKEND (9)  =   1 )
      *
                   IF    ( TSRYKA-SRYCD (IDX6 IDX7)(1:1)   NOT =   "6" )
                    AND  ( TSRYKA-SRYCD (IDX6 IDX7)(1:1)   NOT =   "S" )
                       MOVE    "09"    TO  WRK-REIGAI-CHKNUM
                       MOVE    "2"     TO  WRK-REIGAI-CHKKBN
                       MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-REIGAI-CD
                       PERFORM 800-REIGAI-CHK-SEC
                       IF    ( FLG-REIGAI  =   ZERO )
                           MOVE    "2"         TO  WRK-CHK-CHKKBN
                           MOVE    "1"         TO  WRK-CHK-CDKBN
                           MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                           TO  WRK-CHK-SRYCD
                           PERFORM 900-DTCHK008-READ-SEC
                           IF    ( FLG-DTCHK008    =   1 )
                               PERFORM 200112-CHKMST-SRY-BYO-CHK-SEC
                           END-IF
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           .
       2001121-SRY-BYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為と病名チェック処理（チェックマスタチェック）
      *****************************************************************
       200112-CHKMST-SRY-BYO-CHK-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           MOVE    "2"                 TO  WRK-CHK-CHKKBN
           MOVE    "M"                 TO  WRK-CHK-CDKBN
           MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-CHK-SRYCD
           PERFORM 900-DTCHK008-READ-SEC
           IF    ( FLG-DTCHK008        =   ZERO )
               CONTINUE
           ELSE
               PERFORM 900-DTCHK008-WRITE-SEC
      *
               MOVE    "2"             TO  WRK-CHK-CHKKBN
               MOVE    "1"             TO  WRK-CHK-CDKBN
               MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-CHK-SRYCD
               PERFORM 800-TMPSND-EDIT-SEC
           END-IF
      *
           INITIALIZE                      CHKSND-REC
           MOVE    "2"                 TO  CHKSND-CHKKBN
           MOVE    "0"                 TO  CHKSND-CDKBN
           MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  CHKSND-SRYCD
           MOVE    WRK-KJNYMD          TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM    TO  CHKSND-HOSPNUM
           PERFORM 901-CHKSND-KEY3-SEL-SEC
           IF    ( FLG-CHKSND           =   ZERO )
               IF    ( CHKSND-NYUGAIKBN    =   SPACE )
                OR   ( CHKSND-NYUGAIKBN    =   ORCBSD01-NYUGAIKBN )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-CHKMST
               END-IF
           END-IF
      *
           INITIALIZE                      CHKSND-REC
           MOVE    "2"                 TO  CHKSND-CHKKBN
           MOVE    "1"                 TO  CHKSND-CDKBN
           MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  CHKSND-SRYCD
           MOVE    WRK-KJNYMD          TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM    TO  CHKSND-HOSPNUM
           PERFORM 901-CHKSND-KEY3-SEL-SEC
           IF    ( FLG-CHKSND           =   ZERO )
               CONTINUE
           ELSE
                   MOVE    1           TO  FLG-CHKMST
           END-IF
      *
           IF    ( FLG-CHKMST          =   ZERO )
      *
               INITIALIZE                  SDTCHK01AREA
               IF    ( SYS-1101-BYOCHKPTN-G    =   SPACE )
                   MOVE    CONST-CHKPTN    TO  SDTCHK01-CHKPTN-G
               ELSE
                   MOVE    SYS-1101-BYOCHKPTN-G
                                       TO  SDTCHK01-CHKPTN-G
               END-IF
               MOVE    DTCHK003-HOSPNUM TO  SDTCHK01-HOSPNUM
               MOVE    DTCHK003-PTID   TO  SDTCHK01-PTID
               MOVE    TSRYKA-SRYKA (IDX6)
                                       TO  SDTCHK01-SRYKA
               MOVE    CHKSND-CHKKBN   TO  SDTCHK01-CHKKBN
               MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  SDTCHK01-SRYCD
               MOVE    WRK-KJNYMD      TO  SDTCHK01-KJNYMD
               MOVE    "1"             TO  SDTCHK01-CDKBN
               MOVE    ORCBSD01-NYUGAIKBN
                                       TO  SDTCHK01-NYUGAIKBN
               MOVE    DTCHK003-HKNKBN TO  SDTCHK01-HKNKBN
               MOVE    CHKSND-GRPCHKKBN
                                           TO  SDTCHK01-GRPCHKKBN
               MOVE    TSRYKA-SRYCD-DAY-G (IDX6 IDX7)
                                       TO  SDTCHK01-CHKDAY-G
               CALL    "ORCSDTCHK01"   USING   SDTCHK01AREA
                                               SYS-1101-REC
                                               SDTCHK02AREA
                                               LNK-HKNCOMBI-AREA
                                               SPA-AREA
                                               DTCHKCOMMON-AREA
               IF    ( SDTCHK01-O-OK   =   ZERO )
                   MOVE    TSRYKA-SRYKA (IDX6)
                                       TO  WRK-SRYKA
                   INITIALIZE              TERRINF-AREA
                   PERFORM 200112-SRY-BYO-ERRMES-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-CHKMST
               MOVE    "2"             TO  WRK-CHK-CHKKBN
               MOVE    "1"             TO  WRK-CHK-CDKBN
               MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-CHK-SRYCD
               PERFORM 900-DTCHK008-WRITE-SEC
           END-IF
      *
           .
       200112-CHKMST-SRY-BYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力セットと病名チェック処理
      *****************************************************************
       2001121-PSET-BYO-CHK-SEC        SECTION.
      *
           INITIALIZE                  CHKSND-REC
           MOVE    SPA-HOSPNUM     TO  CHKSND-HOSPNUM
           MOVE    "2"             TO  CHKSND-CHKKBN
           MOVE    "P00000"        TO  CHKSND-SRYCD
           MOVE    "1"             TO  CHKSND-CDKBN
           PERFORM 901-DTCHK010-START-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK010    NOT =   ZERO )
      *
               MOVE    DTCHK010-DAT    TO  CHKSND-REC
      *
               IF    ( CHKSND-NYUGAIKBN    =   SPACE )
                OR   ( CHKSND-NYUGAIKBN    =   ORCBSD01-NYUGAIKBN )
      *
                   MOVE    "09"    TO  WRK-REIGAI-CHKNUM
                   MOVE    "2"     TO  WRK-REIGAI-CHKKBN
                   MOVE    CHKSND-SRYCD
                                   TO  WRK-REIGAI-CD
                   PERFORM 800-REIGAI-CHK-SEC
                   IF    ( FLG-REIGAI  =   ZERO )
                       PERFORM 20011211-PSET-BYO-CHK-SEC
                   END-IF
               END-IF
      *
               PERFORM 901-DTCHK010-READ-SEC
      *
           END-PERFORM
      *
           .
       2001121-PSET-BYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力セットと病名チェック処理
      *****************************************************************
       20011211-PSET-BYO-CHK-SEC       SECTION.
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >       TSRYKA-MAX )
      *
               INITIALIZE          TPSET-AREA
      *
               PERFORM VARYING IDXD FROM    1   BY  1
                       UNTIL ( IDXD >   31 )
      *
                   MOVE    DTCHK010-SETCD-MAX (IDXD)
                                           TO  TPSET-MAX
                   PERFORM VARYING IDX7    FROM    1   BY  1
                           UNTIL ( IDX7    >   TSRYKA-PSRYCD-MAX (IDX6))
      *
                       IF    ( TSRYKA-PSRYCD-DAY(IDX6 IDX7 IDXD)
                                           >   ZERO)
                           PERFORM VARYING IDXP    FROM    1   BY  1
                                   UNTIL ( IDXP    >   TPSET-MAX )
      *
                               IF    ( TSRYKA-PSRYCD (IDX6 IDX7)
                                       =   DTCHK010-SETCD1 (IDXD IDXP)
                                        OR DTCHK010-SETCD2 (IDXD IDXP))
                                   MOVE    1   TO  TPSET-FLG
                                                          (IDXD IDXP)
                               END-IF
      *
                           END-PERFORM
                       END-IF
                   END-PERFORM
               END-PERFORM
      *
               PERFORM VARYING IDXD FROM    1   BY  1
                       UNTIL ( IDXD >   31 )
      *
                   MOVE    DTCHK010-SETCD-MAX (IDXD)
                                           TO  TPSET-MAX
      *
                   IF    ( TPSET-MAX       >   ZERO )
                    AND  ( TPSET-FLG-OCC (IDXD)(1: TPSET-MAX)
                                               =   ALL "1" )
                       MOVE    1       TO  TPSET-DAY (IDXD)
                       
                   END-IF
      *
               END-PERFORM
      *
               IF    ( TPSET-DAY-G     NOT =   ZERO )
      *
                   INITIALIZE                  SDTCHK01AREA
                   IF    ( SYS-1101-BYOCHKPTN-G    =   SPACE )
                       MOVE    CONST-CHKPTN
                                           TO  SDTCHK01-CHKPTN-G
                   ELSE
                       MOVE    SYS-1101-BYOCHKPTN-G
                                           TO  SDTCHK01-CHKPTN-G
                   END-IF
                   MOVE    DTCHK003-HOSPNUM TO  SDTCHK01-HOSPNUM
                   MOVE    DTCHK003-PTID   TO  SDTCHK01-PTID
                   MOVE    TSRYKA-SRYKA (IDX6)
                                           TO  SDTCHK01-SRYKA
                   MOVE    CHKSND-CHKKBN   TO  SDTCHK01-CHKKBN
                   MOVE    CHKSND-SRYCD    TO  SDTCHK01-SRYCD
                   MOVE    WRK-KJNYMD      TO  SDTCHK01-KJNYMD
                   MOVE    "1"             TO  SDTCHK01-CDKBN
                   MOVE    ORCBSD01-NYUGAIKBN
                                           TO  SDTCHK01-NYUGAIKBN
                   MOVE    DTCHK003-HKNKBN TO  SDTCHK01-HKNKBN
                   MOVE    CHKSND-GRPCHKKBN
                                           TO  SDTCHK01-GRPCHKKBN
                   MOVE    TPSET-DAY-G     TO  SDTCHK01-CHKDAY-G
                   CALL    "ORCSDTCHK01"   USING   SDTCHK01AREA
                                                   SYS-1101-REC
                                                   SDTCHK02AREA
                                                   LNK-HKNCOMBI-AREA
                                                   SPA-AREA
                                                   DTCHKCOMMON-AREA
                   IF    ( SDTCHK01-O-OK   =   ZERO )
                       MOVE    TSRYKA-SRYKA (IDX6)
                                           TO  WRK-SRYKA
                       INITIALIZE              TERRINF-AREA
                       PERFORM 200112-SRY-BYO-ERRMES-SEC
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       20011211-PSET-BYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為と病名エラーメッセージ編集処理
      *****************************************************************
       200112-SRY-BYO-ERRMES-SEC       SECTION.
      *
           IF    ( SDTCHK01-O-NODAY-G      NOT =   ZERO )
      *
               INITIALIZE  WRK-NISSU-AREA
               MOVE    SDTCHK01-O-NODAY-G  TO  WRK-NISSU-DAY-G
      *
               PERFORM 200111-NISSU-HEN-SEC
      *
               MOVE    CHKSND-SRYCD    TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  WRK-SRY-NAME
      *
               IF    ( CHKSND-SRYCD(1:1)   =   "S" OR "P" )
                   MOVE    CHKSND-SRYCD        TO  WRK-SRYCD
                   PERFORM 900-INPUTCD-KEY2-SEL-SEC
                   IF    ( FLG-INPUTCD         =   ZERO )
                       MOVE    ICD-DSPNAME     TO  WRK-SRY-NAME
                   END-IF
               ELSE
      *
                   MOVE    CHKSND-SRYCD        TO  WRK-SRYCD
                   MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU           =   ZERO )
                       MOVE    TNS-NAME        TO  WRK-SRY-NAME
                   END-IF
               END-IF
      *
               MOVE    "090001"            TO  WRK-ERRCD
      *
               MOVE   SPACE                TO  WRK-ERRMES-PLUS
               STRING WRK-SRY-NAME             DELIMITED BY SPACE
                      WRK-NISSU-MES            DELIMITED BY SPACE
               INTO   WRK-ERRMES-PLUS
               END-STRING
      *
               MOVE    WRK-SRYKA           TO  SRH-1005-SRYKA
               INITIALIZE                      TERRINF-AREA
               MOVE    "2"                 TO  TERRINF-CHKKBN
               MOVE    CHKSND-SRYCD        TO  TERRINF-SRYCD
               MOVE    WRK-SRYKA           TO  TERRINF-SRYKA
               MOVE    WRK-NISSU-DAY-G     TO  TERRINF-DAY-G
               MOVE    WRK-NISSU-LASTYMD   TO  TERRINF-LASTYMD
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (9) =   "1" )
                   MOVE    1       TO  FLG-CHKEND (9)
               END-IF
           END-IF
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SDTCHK01-O-MAX )
                        OR   ( FLG-CHKEND (9)  =   1 )
      *
                   INITIALIZE  WRK-NISSU-AREA
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
                       IF    ( SDTCHK01-O-ERRDAY (IDX0 IDX-DAY) =   1 )
                           MOVE    1       TO  WRK-NISSU-DAY (IDX-DAY)
                       END-IF
                   END-PERFORM
      *
                   IF    ( WRK-NISSU-DAY-G NOT =   ZERO )
                       MOVE    "090004"   TO  WRK-ERRCD
                       PERFORM 2001121-SRY-BYO-ERRMES-SEC
                   END-IF
      *
                   INITIALIZE  WRK-NISSU-AREA
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
                       IF    ( SDTCHK01-O-ERRDAY (IDX0 IDX-DAY) =   2 )
                           MOVE    1       TO  WRK-NISSU-DAY (IDX-DAY)
                       END-IF
                   END-PERFORM
                   IF    ( WRK-NISSU-DAY-G NOT =   ZERO )
                       IF    ( ORCBSD01-NYUGAIKBN  =  "1" )
                           MOVE    "090002"   TO  WRK-ERRCD
                       ELSE
                           MOVE    "090003"   TO  WRK-ERRCD
                       END-IF
                       PERFORM 2001121-SRY-BYO-ERRMES-SEC
                   END-IF
               END-PERFORM
      *
           .
       200112-SRY-BYO-ERRMES-EXT.
           EXIT.
      *****************************************************************
      *    診療行為と病名エラーメッセージ編集処理
      *****************************************************************
       2001121-SRY-BYO-ERRMES-SEC   SECTION.
      *
           PERFORM 200111-NISSU-HEN-SEC
      *
           MOVE    CHKSND-SRYCD    TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-SRY-NAME
      *
           IF    ( CHKSND-SRYCD(1:1)   =   "S" OR "P" )
               MOVE    CHKSND-SRYCD        TO  WRK-SRYCD
               PERFORM 900-INPUTCD-KEY2-SEL-SEC
               IF    ( FLG-INPUTCD         =   ZERO )
                   MOVE    ICD-DSPNAME     TO  WRK-SRY-NAME
               END-IF
           ELSE
      *
               MOVE    CHKSND-SRYCD        TO  WRK-SRYCD
               MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU           =   ZERO )
                   MOVE    TNS-NAME        TO  WRK-SRY-NAME
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  WRK-ERRMES-PLUS
           IF    ( SDTCHK01-O-HOSOKU (IDX0)
                                       =   SPACE )
               STRING  WRK-SRY-NAME        DELIMITED BY SPACE
                       "、"                DELIMITED BY SIZE
                       SDTCHK01-O-BYOMEI (IDX0)
                                           DELIMITED BY SPACE
                       WRK-NISSU-MES       DELIMITED BY SPACE
               INTO    WRK-ERRMES-PLUS
               END-STRING
           ELSE
               STRING  WRK-SRY-NAME        DELIMITED BY SPACE
                       "、"                DELIMITED BY SIZE
                       SDTCHK01-O-BYOMEI (IDX0)
                                           DELIMITED BY SPACE
                       "（"                DELIMITED BY SIZE
                       SDTCHK01-O-HOSOKU (IDX0)
                                           DELIMITED BY SPACE
                       "）"                DELIMITED BY SIZE
                       WRK-NISSU-MES       DELIMITED BY SPACE
               INTO    WRK-ERRMES-PLUS
               END-STRING
           END-IF
      *
           MOVE    WRK-SRYKA       TO  SRH-1005-SRYKA
           INITIALIZE                      TERRINF-AREA
           MOVE    "2"                 TO  TERRINF-CHKKBN
           MOVE    CHKSND-SRYCD        TO  TERRINF-SRYCD
           MOVE    WRK-SRYKA           TO  TERRINF-SRYKA
           MOVE    WRK-NISSU-DAY-G     TO  TERRINF-DAY-G
           MOVE    WRK-NISSU-LASTYMD   TO  TERRINF-LASTYMD
           PERFORM 213-ERRMES-HEN-SEC
      *
           IF    ( ORCBSD01-CHKKBN (9) =   "1" )
               MOVE    1           TO  FLG-CHKEND (9)
           END-IF
      *
           .
       2001121-SRY-BYO-ERRMES-EXT.
           EXIT.
      *****************************************************************
      *    併用算定の診療行為チェック処理
      *****************************************************************
       200111-HEI-SRY-CHK-SEC          SECTION.
      *
           IF    ( ORCBSD01-CHKKBN(11) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (11)
           END-IF
      *
      *    併用算定の診療行為チェック処理
           IF    ( FLG-CHKEND (11)     =   ZERO )
            AND  ( FLG-SRYHEI          =   1 )
                   PERFORM 200112-HEI-SRY-CHK-SEC
           END-IF
      *
           .
       200111-HEI-SRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用禁忌の薬剤チェック処理
      *****************************************************************
       200112-HEI-YKZ-CHK-SEC          SECTION.
      *
           INITIALIZE                  THEIYOU-AREA
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   TSRYCD-MAX )
                    OR   ( FLG-CHKEND (10)     =   1 )
      *
      D        DISPLAY PTNUM-PTNUM ":SRYCD[" TSRYCD-SRYCD (IDX6) "]"
      *
               IF    ( TSRYCD-SRYCD (IDX6)(1:1)    =   "6" )
      *
      *            相互作用テーブルよりチェック
                   IF    ( FLG-CHKEND (10)     =   ZERO )
                       IF    ( SYS-1101-YKZHEIMSTKBN   =   "1" )
                           PERFORM 200112-INTERACT-CHK-SEC
                       END-IF
                   END-IF
      *
      *            チェックマスタよりチェック
                   IF    ( FLG-CHKEND (10)     =   ZERO )
                       MOVE    "4"             TO  WRK-CHK-CHKKBN
                       MOVE    "2"             TO  WRK-CHK-CDKBN
                       MOVE    TSRYCD-SRYCD (IDX6)
                                               TO  WRK-CHK-SRYCD
                       PERFORM 900-DTCHK008-READ-SEC
                       IF    ( FLG-DTCHK008    =   1 )
                           PERFORM 200112-CHKMST-HEI-YKZ-CHK-SEC
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    エラーメッセージ編集処理
           IF    ( THEIYOU-MAX         >   ZERO )
               PERFORM 200112-HEI-YKZ-ERRMES-HEN-SEC
           END-IF
      *
           .
       200112-HEI-YKZ-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用禁忌の薬剤チェック処理（チェックマスタ）
      *****************************************************************
       200112-CHKMST-HEI-YKZ-CHK-SEC   SECTION.
      *
           MOVE    "4"                 TO  TCHK-CHK-CHKKBN
           MOVE    TSRYCD-SRYCD (IDX6) TO  TCHK-CHK-SRYCD
           MOVE    WRK-KJNYMD          TO  TCHK-CHK-YMD
           MOVE    "2"                 TO  TCHK-CHK-CDKBN
           PERFORM 230-CHKMST-GET-SEC
           IF    ( TCHK-ACT-CD-MAX     >   ZERO )
               CONTINUE
           ELSE
               MOVE    "4"             TO  WRK-CHK-CHKKBN
               MOVE    "2"             TO  WRK-CHK-CDKBN
               MOVE    TSRYCD-SRYCD (IDX6)
                                       TO  WRK-CHK-SRYCD
               PERFORM 900-DTCHK008-WRITE-SEC
           END-IF
      *
      *    チェックマスタより診療行為チェック処理
           PERFORM VARYING IDX10   FROM    1   BY  1
                   UNTIL ( IDX10           >   TCHK-ACT-CD-MAX )
                      OR ( FLG-CHKEND (10) =  1 )
      *
               MOVE    TCHK-ACT-CHKKBN     TO  WRK-CHKKBN
               MOVE    IDX6                TO  IDX-TSRYCD
               MOVE    TCHK-ACT-CD (IDX10) TO  WRK-CHK-CD
               MOVE    TCHK-ACT-NYUGAIBNDKBN (IDX10)
                                           TO  WRK-NYUGAIBNDKBN
               MOVE    ORCBSD01-CHKKBN (10)
                                           TO  WRK-ORCBSD01-CHKKBN
               MOVE    TCHK-ACT-SYORIKBN (IDX10)
                                           TO  WRK-SYORIKBN
               MOVE    FLG-CHKEND (10)     TO  WRK-FLG-CHKEND
               PERFORM 200112-CHKMST-SRY-CHK-SEC
               MOVE    WRK-FLG-CHKEND      TO  FLG-CHKEND (10)
      *
           END-PERFORM
      *
      *
           .
       200112-CHKMST-HEI-YKZ-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ診療行為チェック処理
      *****************************************************************
       200112-CHKMST-SRY-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-HIT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   THEIYOU-MAX )
                     OR  ( FLG-HIT =   1 )
      *
               IF   ( WRK-CHKKBN   =   THEIYOU-CHKKBN(IDX0) )
                 AND( THEIYOU-INTERACT (IDX0)
                                   =   SPACE )
                 AND( TSRYCD-SRYCD (IDX-TSRYCD)
                                   =   THEIYOU-SRYCD1(IDX0) )
                 AND( WRK-CHK-CD
                                   =   THEIYOU-SRYCD2(IDX0) )
                   MOVE    1       TO  FLG-HIT
               END-IF
      *
               IF   ( WRK-CHKKBN   =   THEIYOU-CHKKBN(IDX0) )
                 AND( THEIYOU-INTERACT (IDX0)
                                   =   SPACE )
                 AND( TSRYCD-SRYCD (IDX-TSRYCD)
                                   =   THEIYOU-SRYCD2(IDX0) )
                 AND( WRK-CHK-CD
                                   =   THEIYOU-SRYCD1(IDX0) )
                   MOVE    1       TO  FLG-HIT
               END-IF
      *
           END-PERFORM
      *
      *    併用禁忌（算定）退避ワークに存在する場合は、後続の処理は
      *    行わない
           IF    ( FLG-HIT         =   1 )
               GO  TO  200112-CHKMST-SRY-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TSRYCD-MAX )
                    OR   ( WRK-FLG-CHKEND      =   1 )
      *
               EVALUATE    WRK-CHKKBN
               WHEN    "4"
               WHEN    "5"
                   PERFORM 20011211-HEIYOU-M-SET-CHK-SEC
               WHEN    "A"
                   PERFORM 20011211-HEIYOU-D-SET-CHK-SEC
               WHEN    "B"
                   PERFORM 20011211-HEIYOU-T-SET-CHK-SEC
               END-EVALUATE
      *
               IF    ( FLG-THEIYOU-SET =   1 )
                   IF    ( CONST-THEIYOU-MAX   > THEIYOU-MAX )
                           COMPUTE THEIYOU-MAX =   THEIYOU-MAX +   1
      *
                       MOVE    WRK-CHKKBN
                                       TO  THEIYOU-CHKKBN
                                                         (THEIYOU-MAX)
                       MOVE    TSRYCD-NYUGAIKBN (IDX-TSRYCD)
                                       TO  THEIYOU-NYUGAIKBN1
                                                         (THEIYOU-MAX)
                       MOVE    TSRYCD-SRYCD (IDX-TSRYCD)
                                       TO  THEIYOU-SRYCD1(THEIYOU-MAX)
                       MOVE    FLG-SRYHEI1-DAY-G
                                       TO  THEIYOU-DAY1-G (THEIYOU-MAX)
      *
                       MOVE    TSRYCD-NYUGAIKBN (IDX0)
                                       TO  THEIYOU-NYUGAIKBN2
                                                         (THEIYOU-MAX)
                       MOVE    TSRYCD-SRYCD (IDX0)
                                       TO  THEIYOU-SRYCD2(THEIYOU-MAX)
                       MOVE    FLG-SRYHEI2-DAY-G
                                       TO  THEIYOU-DAY2-G (THEIYOU-MAX)
      *
                       IF    ( WRK-ORCBSD01-CHKKBN =   "1" )
                           MOVE    1   TO  WRK-FLG-CHKEND
                       END-IF
                    END-IF
      *
                   MOVE    TSRYCD-MAX      TO  IDX0
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200112-CHKMST-SRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定エラー設定対象チェック処理（同一月）
      *****************************************************************
       20011211-HEIYOU-M-SET-CHK-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYHEI1-DAY-G
                                       FLG-SRYHEI2-DAY-G
           MOVE    1               TO  FLG-THEIYOU-SET
      *
           IF    ( WRK-CHK-CD      =   TSRYCD-SRYCD (IDX0) )
               IF    ( IDX-TSRYCD  =   IDX0 )
                   IF    ( TSRYCD-KAISU (IDX0)
                                   >   1 )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-THEIYOU-SET
                   END-IF
               END-IF
           ELSE
                       MOVE    ZERO    TO  FLG-THEIYOU-SET
           END-IF
      *
           IF  (( TSRYCD-NYUGAIKBN(IDX-TSRYCD)
                                       =   ORCBSD01-NYUGAIKBN )
            AND ( TSRYCD-HKNFLG   (IDX-TSRYCD)
                                       =   1 ))
           OR  (( TSRYCD-NYUGAIKBN(IDX0)
                                       =   ORCBSD01-NYUGAIKBN )
            AND ( TSRYCD-HKNFLG   (IDX0)
                                       =   1 ))
                CONTINUE
           ELSE
                MOVE    ZERO    TO  FLG-THEIYOU-SET
           END-IF
      *
           IF    ( FLG-THEIYOU-SET     =   1 )
      *
               EVALUATE    WRK-CHKKBN
               WHEN    "4"
                   MOVE    TSRYCD-DAY-G (IDX-TSRYCD)
                                       TO  FLG-SRYHEI1-DAY-G
                   MOVE    TSRYCD-DAY-G (IDX0)
                                       TO  FLG-SRYHEI2-DAY-G
               WHEN    "5"
                   EVALUATE    TRUE
                   WHEN  ( WRK-SYORIKBN              =   "1" )
                    AND  ( TSRYCD-SRYCD (IDX-TSRYCD) =   "113006510" )
                    AND  ( TSRYCD-SRYCD (IDX0)   NOT =   "113006510" )
      *
                       MOVE    IDX-TSRYCD      TO  IDXS1
                       MOVE    1               TO  IDXD1
                       MOVE    IDX0            TO  IDXS2
                       MOVE    2               TO  IDXD2
                       PERFORM 200112111-HEIYOU-M-SET-CHK-SEC
      *
                   WHEN  ( WRK-SYORIKBN              =   "1" )
                    AND  ( TSRYCD-SRYCD (IDX-TSRYCD)
                                                 NOT =   "113006510" )
                    AND  ( TSRYCD-SRYCD (IDX0)       =   "113006510" )
      *
                       MOVE    IDX-TSRYCD      TO  IDXS2
                       MOVE    1               TO  IDXD2
                       MOVE    IDX0            TO  IDXS1
                       MOVE    2               TO  IDXD1
                       PERFORM 200112111-HEIYOU-M-SET-CHK-SEC
      *
                   WHEN  ( WRK-SYORIKBN              =   "2" )
                    AND  ( TSRYCD-SRYCD (IDX-TSRYCD) =   "111000110"
                                                      OR "111012510"
                                                      OR "111013850" )
                    AND  ( TSRYCD-SRYCD (IDX0)   NOT =   "111000110" )
                    AND  ( TSRYCD-SRYCD (IDX0)   NOT =   "111012510" )
                    AND  ( TSRYCD-SRYCD (IDX0)   NOT =   "111013850" )
      *
                       MOVE    IDX-TSRYCD      TO  IDXS1
                       MOVE    1               TO  IDXD1
                       MOVE    IDX0            TO  IDXS2
                       MOVE    2               TO  IDXD2
                       PERFORM 200112112-HEIYOU-M-SET-CHK-SEC
      *
                   WHEN  ( WRK-SYORIKBN              =   "2" )
                    AND  ( TSRYCD-SRYCD (IDX-TSRYCD)
                                                 NOT =   "111000110" )
                    AND  ( TSRYCD-SRYCD (IDX-TSRYCD)
                                                 NOT =   "111012510" )
                    AND  ( TSRYCD-SRYCD (IDX-TSRYCD)
                                                 NOT =   "111013850" )
                    AND  ( TSRYCD-SRYCD (IDX0)       =   "111000110"
                                                      OR "111012510"
                                                      OR "111013850" )
      *
                       MOVE    IDX-TSRYCD      TO  IDXS2
                       MOVE    1               TO  IDXD2
                       MOVE    IDX0            TO  IDXS1
                       MOVE    2               TO  IDXD1
                       PERFORM 200112112-HEIYOU-M-SET-CHK-SEC
      *
                   WHEN    OTHER
                       MOVE    TSRYCD-DAY-G (IDX-TSRYCD)
                                           TO  FLG-SRYHEI1-DAY-G
                       MOVE    TSRYCD-DAY-G (IDX0)
                                           TO  FLG-SRYHEI2-DAY-G
                   END-EVALUATE
               END-EVALUATE
      *
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( WRK-NYUGAIBNDKBN    =   "1" )
                       IF    ( TSRYCD-DAY-NYUGAIKBN (IDX-TSRYCD IDX-DAY)
                                          NOT  =   ORCBSD01-NYUGAIKBN )
                           MOVE    ZERO TO  FLG-SRYHEI1-DAY (IDX-DAY)
                       END-IF
                       IF    ( TSRYCD-DAY-NYUGAIKBN (IDX0 IDX-DAY)
                                          NOT  =   ORCBSD01-NYUGAIKBN )
                           MOVE    ZERO TO  FLG-SRYHEI2-DAY (IDX-DAY)
                       END-IF
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-SRYHEI1-DAY-G       =   ZERO )
                OR   ( FLG-SRYHEI2-DAY-G       =   ZERO )
                    MOVE    ZERO    TO  FLG-THEIYOU-SET
               END-IF
           END-IF
      *
           .
       20011211-HEIYOU-M-SET-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定エラー設定対象チェック処理
      *    （慢性疼痛疾患管理料の特例チェック）
      *****************************************************************
       200112111-HEIYOU-M-SET-CHK-SEC  SECTION.
      *
           MOVE    TSRYCD-DAY-G(IDXS1) TO  FLG-SRYHEI-DAY-OCC (IDXD1)
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               IF  ( TSRYCD-DAY (IDXS2 IDX-DAY)
                                       =   1 )
                   MOVE ORCBSD01-SRYYM TO  WRK-YMD
                   MOVE IDX-DAY        TO  WRK-DD
                   IF ( WRK-YMD >=  TSRYCD-FSANTEIYMD (IDXS1))
                       MOVE    1       TO FLG-SRYHEI-DAY (IDXD2 IDX-DAY)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200112111-HEIYOU-M-SET-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定エラー設定対象チェック処理
      *    （初診料と特定疾患療養管理料）
      *****************************************************************
       200112112-HEIYOU-M-SET-CHK-SEC   SECTION.
      *
           MOVE    TSRYCD-DAY-G(IDXS1) TO  FLG-SRYHEI-DAY-OCC (IDXD1)
      *
           MOVE    ZERO        TO  IDXD
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
                    OR   ( IDXD    >   ZERO )
      *
               IF    ( TSRYCD-DAY(IDXS1 IDX-DAY)   =   1 )
                   MOVE    IDX-DAY TO  IDXD
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDXD            >   ZERO )
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
      *
                   IF  ( TSRYCD-DAY (IDXS2 IDX-DAY)
                                       =   1 )
                       IF ( IDX-DAY >= IDXD )
                           MOVE    1
                                   TO  FLG-SRYHEI-DAY (IDXD2 IDX-DAY)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       200112112-HEIYOU-M-SET-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定エラー設定対象チェック処理（同一日）
      *****************************************************************
       20011211-HEIYOU-D-SET-CHK-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYHEI1-DAY-G
                                       FLG-SRYHEI2-DAY-G
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               MOVE    1           TO  FLG-THEIYOU-SET
      *
               IF    ( WRK-CHK-CD      =   TSRYCD-SRYCD (IDX0) )
                   IF    ( IDX-TSRYCD  =   IDX0 )
                       IF    ( TSRYCD-DAY-KAISU (IDX0 IDX-DAY)
                                   >   1 )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-THEIYOU-SET
                       END-IF
                   ELSE
                       IF    ( TSRYCD-DAY-KAISU (IDX-TSRYCD IDX-DAY)
                                   >   ZERO )
                        AND  ( TSRYCD-DAY-KAISU (IDX0 IDX-DAY)
                                   >   ZERO )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-THEIYOU-SET
                       END-IF
                   END-IF
               ELSE
                   MOVE    ZERO    TO  FLG-THEIYOU-SET
               END-IF
      *
               IF    ( WRK-NYUGAIBNDKBN    =   "1" )
                   IF    ( TSRYCD-DAY-NYUGAIKBN(IDX-TSRYCD IDX-DAY)
                                           =   ORCBSD01-NYUGAIKBN )
                    AND  ( TSRYCD-DAY-NYUGAIKBN(IDX0       IDX-DAY)
                                           =   ORCBSD01-NYUGAIKBN )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-THEIYOU-SET
                   END-IF
               END-IF
      *
               IF   (( TSRYCD-DAY-NYUGAIKBN(IDX-TSRYCD IDX-DAY)
                                           =   ORCBSD01-NYUGAIKBN )
                 AND ( TSRYCD-DAY-HKNFLG   (IDX-TSRYCD IDX-DAY)
                                           =   1 ))
                OR  (( TSRYCD-DAY-NYUGAIKBN(IDX0 IDX-DAY)
                                           =   ORCBSD01-NYUGAIKBN )
                 AND ( TSRYCD-DAY-HKNFLG   (IDX0 IDX-DAY)
                                           =   1 ))
                    CONTINUE
               ELSE
                   MOVE    ZERO    TO  FLG-THEIYOU-SET
               END-IF
      *
               IF    ( FLG-THEIYOU-SET     =   1 )
                   MOVE    1       TO  FLG-SRYHEI1-DAY (IDX-DAY)
                                       FLG-SRYHEI2-DAY (IDX-DAY)
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-SRYHEI1-DAY-G       =   ZERO )
            OR   ( FLG-SRYHEI2-DAY-G       =   ZERO )
                MOVE    ZERO       TO  FLG-THEIYOU-SET
           ELSE
                MOVE    1          TO  FLG-THEIYOU-SET
           END-IF
      *
           .
       20011211-HEIYOU-D-SET-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定エラー設定対象チェック処理（同一会計）
      *****************************************************************
       20011211-HEIYOU-T-SET-CHK-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYHEI1-DAY-G
                                       FLG-SRYHEI2-DAY-G
      *
           MOVE    1               TO  FLG-THEIYOU-SET
      *
           IF    ( WRK-CHK-CD      =   TSRYCD-SRYCD (IDX0) )
               PERFORM VARYING IDXI    FROM    1   BY  1
                       UNTIL ( IDXI  > CONST-GROUP-MAX )
                   IF    ( TSRYCD-GROUP (IDX-TSRYCD IDXI)
                               NOT =   ZERO )
                    AND  ( TSRYCD-GROUP (IDX-TSRYCD IDXI)
                                   =   TSRYCD-GROUP  (IDX0 IDXI))
                       PERFORM 200112111-HEIYOU-T-SET-CHK-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
           IF    ( FLG-SRYHEI1-DAY-G       =   ZERO )
            OR   ( FLG-SRYHEI2-DAY-G       =   ZERO )
               MOVE    ZERO        TO  FLG-THEIYOU-SET
           END-IF
      *
           .
       20011211-HEIYOU-T-SET-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定エラー設定対象チェック処理（同一会計）
      *****************************************************************
       200112111-HEIYOU-T-SET-CHK-SEC   SECTION.
      *
           MOVE    1               TO  FLG-TARGET
      *
           IF    ( IDX-TSRYCD  =   IDX0 )
               IF    ( TSRYCD-GROUP-KAISU (IDX0 IDXI)
                                   >   1 )
                   CONTINUE
               ELSE
                   MOVE    ZERO    TO  FLG-TARGET
               END-IF
           ELSE
               IF    ( TSRYCD-GROUP-KAISU (IDX-TSRYCD IDXI)
                                   >   ZERO )
               AND   ( TSRYCD-GROUP-KAISU (IDX0 IDXI)
                                   >   ZERO )
                           CONTINUE
               ELSE
                   MOVE    ZERO    TO  FLG-TARGET
               END-IF
           END-IF
      *
           IF    ( TSRYCD-GROUP-HKNFLG   (IDX-TSRYCD IDXI)
                                           =   1 )
            OR   ( TSRYCD-GROUP-HKNFLG   (IDX0 IDXI)
                                           =   1 )
                    CONTINUE
           ELSE
               MOVE    ZERO        TO  FLG-TARGET
           END-IF
      *
           IF    ( FLG-TARGET     =   1 )
               MOVE    TSRYCD-GROUP-DAY (IDX-TSRYCD IDXI)
                               TO  IDX-DAY
               MOVE    1       TO  FLG-SRYHEI1-DAY (IDX-DAY)
                                   FLG-SRYHEI2-DAY (IDX-DAY)
           END-IF
      *
           .
       200112111-HEIYOU-T-SET-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用禁忌の薬剤チェック処理（相互作用テーブル）
      *****************************************************************
       200112-INTERACT-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-INTERACT
      *
           INITIALIZE                      INTERACT-REC
           MOVE    TSRYCD-SRYCD (IDX6) TO  INTERACT-DRUGCD
           MOVE    INTERACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_interact"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF   (  MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  INTERACT-REC
           ELSE
               MOVE    1               TO  FLG-INTERACT
           END-IF
      *
           PERFORM UNTIL ( FLG-INTERACT    NOT =   ZERO )
                    OR   ( FLG-CHKEND (10)     =   1 )
      *
               MOVE    IDX6            TO  IDX-TSRYCD
               PERFORM 200112-INTERACT-YKZ-CHK-SEC
      *
               MOVE    "tbl_interact"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF   (  MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  INTERACT-REC
               ELSE
                   MOVE    1           TO  FLG-INTERACT
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_interact"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200112-INTERACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    相互作用テーブル薬剤チェック処理
      *****************************************************************
       200112-INTERACT-YKZ-CHK-SEC     SECTION.
      *
      *
           MOVE    ZERO            TO  FLG-HIT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   THEIYOU-MAX )
                    OR   ( FLG-HIT =   1 )
      *
               IF  (( INTERACT-DRUGCD
                                   =   THEIYOU-SRYCD1(IDX0) )
                 AND( INTERACT-DRUGCD2
                                   =   THEIYOU-SRYCD2(IDX0) ))
                   MOVE    1       TO  FLG-HIT
               END-IF
      *
               IF  (( INTERACT-DRUGCD
                                   =   THEIYOU-SRYCD2(IDX0) )
                 AND( INTERACT-DRUGCD2
                                   =   THEIYOU-SRYCD1(IDX0) ))
                   MOVE    1       TO  FLG-HIT
               END-IF
           END-PERFORM
      *
           IF    ( FLG-HIT         =   1 )
               GO  TO  200112-INTERACT-YKZ-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TSRYCD-MAX )
                    OR   ( FLG-CHKEND (10)     =   1 )
      *
               IF    ( IDX-TSRYCD      NOT =   IDX0 )
                AND  ( INTERACT-DRUGCD2    =   TSRYCD-SRYCD (IDX0) )
                   IF    (( TSRYCD-NYUGAIKBN(IDX-TSRYCD)
                                       =   ORCBSD01-NYUGAIKBN )
                      AND ( TSRYCD-HKNFLG   (IDX-TSRYCD)
                                       =   1 ))
                     OR  (( TSRYCD-NYUGAIKBN(IDX0)
                                       =   ORCBSD01-NYUGAIKBN )
                      AND ( TSRYCD-HKNFLG   (IDX0)
                                       =   1 ))
                       IF    ( CONST-THEIYOU-MAX   > THEIYOU-MAX )
                           COMPUTE THEIYOU-MAX =   THEIYOU-MAX +   1
      *
                           MOVE    "4"     TO  THEIYOU-CHKKBN
                                                         (THEIYOU-MAX)
                           MOVE    "1"     TO  THEIYOU-INTERACT
                                                         (THEIYOU-MAX)
                           MOVE    TSRYCD-NYUGAIKBN (IDX-TSRYCD)
                                           TO  THEIYOU-NYUGAIKBN1
                                                         (THEIYOU-MAX)
                           MOVE    TSRYCD-SRYCD (IDX-TSRYCD)
                                           TO  THEIYOU-SRYCD1
                                                         (THEIYOU-MAX)
                           MOVE    TSRYCD-DAY-G (IDX-TSRYCD)
                                       TO  THEIYOU-DAY1-G (THEIYOU-MAX)
      *
                           MOVE    TSRYCD-NYUGAIKBN (IDX0)
                                           TO  THEIYOU-NYUGAIKBN2
                                                         (THEIYOU-MAX)
                           MOVE    TSRYCD-SRYCD (IDX0)
                                           TO  THEIYOU-SRYCD2
                                                         (THEIYOU-MAX)
                           MOVE    TSRYCD-DAY-G (IDX0)
                                       TO  THEIYOU-DAY2-G (THEIYOU-MAX)
                       END-IF
      *
                       IF    ( ORCBSD01-CHKKBN (10)    =   "1" )
                           MOVE    1       TO  FLG-CHKEND (10)
                       END-IF
                   END-IF
      *
                   MOVE    TSRYCD-MAX      TO  IDX0
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200112-INTERACT-YKZ-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用禁忌の薬剤エラーメッセージ編集処理
      *****************************************************************
       200112-HEI-YKZ-ERRMES-HEN-SEC   SECTION.
      *
      *    処理パラメータとは反対の入外を編集
           IF    ( ORCBSD01-NYUGAIKBN   =  "1" )
               MOVE    "（外来）"  TO   WRK-NYUGAI-MES
           ELSE
               MOVE    "（入院）"  TO   WRK-NYUGAI-MES
           END-IF
      *
           IF    ( ORCBSD01-CHKKBN(10)  =  "1" )
               MOVE    1           TO   WRK-MAX
           ELSE
               MOVE    THEIYOU-MAX TO   WRK-MAX
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >    WRK-MAX )
      *
               MOVE    SPACE       TO   WRK-ERRMES-PLUS
               MOVE    SPACE       TO   WRK-LASTYMD
               MOVE    1           TO   WRK-PNT
      *
               IF    ( THEIYOU-NYUGAIKBN1 (IDX0)
                               NOT =    ORCBSD01-NYUGAIKBN )
                   STRING  WRK-NYUGAI-MES  DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   WITH    POINTER    WRK-PNT
                   END-STRING
               END-IF
      *
               MOVE    THEIYOU-SRYCD1 (IDX0)
                                   TO  WRK-SRYCD
               MOVE    WRK-KJNYMD  TO  WRK-TNS-YMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       NOT =   ZERO )
                   MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    WRK-OUT-INPUT   TO  TNS-NAME
               END-IF
      *
               INITIALIZE  WRK-NISSU-AREA
               MOVE    THEIYOU-DAY1-G (IDX0)
                                       TO  WRK-NISSU-DAY-G
               PERFORM 200111-NISSU-HEN-SEC
               MOVE    WRK-NISSU-LASTYMD
                                       TO  WRK-LASTYMD
      *
               STRING TNS-NAME         DELIMITED   BY  SPACE
                      WRK-NISSU-MES    DELIMITED   BY SPACE
                      "、"             DELIMITED   BY  SIZE
               INTO   WRK-ERRMES-PLUS
               WITH    POINTER    WRK-PNT
               END-STRING
      *
               IF    ( THEIYOU-NYUGAIKBN2 (IDX0)
                               NOT =    ORCBSD01-NYUGAIKBN )
                   STRING  WRK-NYUGAI-MES  DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   WITH    POINTER    WRK-PNT
                   END-STRING
               END-IF
      *
      *
               MOVE    THEIYOU-SRYCD2 (IDX0)
                                   TO  WRK-SRYCD
               MOVE    WRK-KJNYMD  TO  WRK-TNS-YMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       NOT =   ZERO )
                   MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    WRK-OUT-INPUT   TO  TNS-NAME
               END-IF
      *
               INITIALIZE  WRK-NISSU-AREA
               MOVE    THEIYOU-DAY2-G (IDX0)
                                       TO  WRK-NISSU-DAY-G
               PERFORM 200111-NISSU-HEN-SEC
      *
               STRING TNS-NAME         DELIMITED   BY  SPACE
                      WRK-NISSU-MES    DELIMITED   BY SPACE
               INTO   WRK-ERRMES-PLUS
               WITH    POINTER    WRK-PNT
               END-STRING
      *
               IF    ( THEIYOU-INTERACT (IDX0) =   SPACE )
                   MOVE    "100001"    TO   WRK-ERRCD
               ELSE
                   MOVE    "100002"    TO   WRK-ERRCD
               END-IF
               INITIALIZE              TERRINF-AREA
               MOVE    "4"         TO  TERRINF-CHKKBN
               MOVE    THEIYOU-SRYCD1 (IDX0)
                                   TO  TERRINF-SRYCD
               MOVE    THEIYOU-SRYCD2 (IDX0)
                                   TO  TERRINF-TEKCD
               MOVE    WRK-LASTYMD TO  TERRINF-LASTYMD
      *
               PERFORM 215-ERRMES-HEN-SEC
      *
           END-PERFORM
      *
           .
       200112-HEI-YKZ-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と投与禁忌病名チェック処理
      *****************************************************************
       200112-HEI-YKZKNKBYO-CHK-SEC    SECTION.
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >       TSRYKA-MAX )
                     OR  ( FLG-CHKEND (15)      =   1 )
      *
               PERFORM VARYING IDX7    FROM    1   BY  1
                       UNTIL ( IDX7    >   TSRYKA-SRYCD-MAX (IDX6))
                         OR  ( FLG-CHKEND (15)  =   1 )
      *
                   IF    ( TSRYKA-SRYCD (IDX6 IDX7)(1:1)   =   "6" )
      *
                       MOVE    "6"     TO  WRK-CHK-CHKKBN
                       MOVE    "1"     TO  WRK-CHK-CDKBN
                       MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-CHK-SRYCD
                       PERFORM 900-DTCHK008-READ-SEC
                       IF    ( FLG-DTCHK008    =   1 )
                           PERFORM 200112-CHKMST-KNKBYO-CHK-SEC
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           .
       200112-HEI-YKZKNKBYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と投与禁忌病名チェック処理（チェックマスタチェック）
      *****************************************************************
       200112-CHKMST-KNKBYO-CHK-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           MOVE    "6"                 TO  WRK-CHK-CHKKBN
           MOVE    "M"                 TO  WRK-CHK-CDKBN
           MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-CHK-SRYCD
           PERFORM 900-DTCHK008-READ-SEC
           IF    ( FLG-DTCHK008        =   ZERO )
               CONTINUE
           ELSE
               PERFORM 900-DTCHK008-WRITE-SEC
      *
               MOVE    "6"             TO  WRK-CHK-CHKKBN
               MOVE    "1"             TO  WRK-CHK-CDKBN
               MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-CHK-SRYCD
               PERFORM 800-TMPSND-EDIT-SEC
           END-IF
      *
           INITIALIZE                      CHKSND-REC
           MOVE    "6"                 TO  CHKSND-CHKKBN
           MOVE    "0"                 TO  CHKSND-CDKBN
           MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  CHKSND-SRYCD
           MOVE    WRK-KJNYMD          TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM    TO  CHKSND-HOSPNUM
           PERFORM 901-CHKSND-KEY3-SEL-SEC
           IF    ( FLG-CHKSND           =   ZERO )
               IF    ( CHKSND-NYUGAIKBN    =   SPACE )
                OR   ( CHKSND-NYUGAIKBN    =   ORCBSD01-NYUGAIKBN )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-CHKMST
               END-IF
           END-IF
      *
           INITIALIZE                      CHKSND-REC
           MOVE    "6"                 TO  CHKSND-CHKKBN
           MOVE    "1"                 TO  CHKSND-CDKBN
           MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  CHKSND-SRYCD
           MOVE    WRK-KJNYMD          TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM    TO  CHKSND-HOSPNUM
           PERFORM 901-CHKSND-KEY3-SEL-SEC
           IF    ( FLG-CHKSND          =   ZERO )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-CHKMST
           END-IF
      *
           IF    ( FLG-CHKMST          =   ZERO )
      *
               INITIALIZE                  SDTCHK01AREA
               IF    ( SYS-1101-BYOCHKPTN-G    =   SPACE )
                   MOVE    CONST-CHKPTN    TO  SDTCHK01-CHKPTN-G
               ELSE
                   MOVE    SYS-1101-BYOCHKPTN-G
                                       TO  SDTCHK01-CHKPTN-G
               END-IF
               MOVE    DTCHK003-HOSPNUM TO  SDTCHK01-HOSPNUM
               MOVE    DTCHK003-PTID   TO  SDTCHK01-PTID
               MOVE    TSRYKA-SRYKA (IDX6)
                                       TO  SDTCHK01-SRYKA
               MOVE    CHKSND-CHKKBN   TO  SDTCHK01-CHKKBN
               MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  SDTCHK01-SRYCD
               MOVE    WRK-KJNYMD      TO  SDTCHK01-KJNYMD
               MOVE    "1"             TO  SDTCHK01-CDKBN
               MOVE    ORCBSD01-NYUGAIKBN
                                       TO  SDTCHK01-NYUGAIKBN
               MOVE    DTCHK003-HKNKBN TO  SDTCHK01-HKNKBN
               MOVE    TSRYKA-SRYCD-DAY-G (IDX6 IDX7)
                                       TO  SDTCHK01-CHKDAY-G
               CALL    "ORCSDTCHK03"   USING   SDTCHK01AREA
                                               SPA-AREA
               IF    ( SDTCHK01-O-OK   =   ZERO )
                   MOVE    TSRYKA-SRYKA (IDX6)
                                       TO  WRK-SRYKA
                   INITIALIZE              TERRINF-AREA
                   PERFORM 200112-KNKBYO-ERRMES-HEN-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-CHKMST
               MOVE    "6"             TO  WRK-CHK-CHKKBN
               MOVE    "1"             TO  WRK-CHK-CDKBN
               MOVE    TSRYKA-SRYCD (IDX6 IDX7)
                                       TO  WRK-CHK-SRYCD
               PERFORM 900-DTCHK008-WRITE-SEC
           END-IF
      *
           .
       200112-CHKMST-KNKBYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    禁忌薬剤と病名エラーメッセージ編集処理
      *****************************************************************
       200112-KNKBYO-ERRMES-HEN-SEC    SECTION.
      *
           MOVE    CHKSND-SRYCD        TO  WRK-SRYCD
           MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  TNS-NAME
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >    SDTCHK01-O-MAX )
                    OR   ( FLG-CHKEND (15) =   1 )
      *
               INITIALIZE  WRK-NISSU-AREA
               MOVE    SDTCHK01-O-ERRDAY-G (IDX0)
                                       TO  WRK-NISSU-DAY-G
      *
               PERFORM 200111-NISSU-HEN-SEC
               MOVE    WRK-NISSU-LASTYMD   TO  WRK-LASTYMD
      *
               MOVE    SPACE               TO  WRK-NYUGAI-MES
               IF  ( SDTCHK01-O-NYUGAIKBN (IDX0)
                                   NOT =   SPACE )
                AND( ORCBSD01-NYUGAIKBN
                                   NOT =   SDTCHK01-O-NYUGAIKBN (IDX0))
                   IF    ( SDTCHK01-O-NYUGAIKBN (IDX0)
                                       =   "1" )
                       MOVE    "（入院）"  TO  WRK-NYUGAI-MES
                   ELSE
                       MOVE    "（外来）"  TO  WRK-NYUGAI-MES
                   END-IF
               END-IF
      *
               MOVE    SPACE           TO  WRK-STR
               IF    ( WRK-SRYKA   NOT =   SDTCHK01-O-SRYKA (IDX0))
                   MOVE    SDTCHK01-O-SRYKA (IDX0)
                                       TO  SRH-1005-SRYKA
                   PERFORM 2111-SRYKA-GET-SEC
                   IF    ( SRH-1005-SRYKAMEI2  NOT =   SPACE )
                       STRING  "［"            DELIMITED   BY  SIZE
                           SRH-1005-SRYKAMEI2  DELIMITED   BY  SPACE
                           "］"                DELIMITED   BY  SIZE
                       INTO    WRK-STR
                       END-STRING
                   END-IF
               END-IF
      *
               MOVE   SPACE            TO  WRK-ERRMES-PLUS
               STRING TNS-NAME             DELIMITED BY SPACE
                      "、"                 DELIMITED BY SIZE
                      WRK-STR              DELIMITED BY SPACE
                      WRK-NYUGAI-MES       DELIMITED BY SPACE
                      SDTCHK01-O-BYOMEI (IDX0)
                                           DELIMITED BY SPACE
                      WRK-NISSU-MES        DELIMITED BY SPACE
               INTO   WRK-ERRMES-PLUS
               END-STRING
      *
               MOVE    "150001"        TO  WRK-ERRCD
      *
               MOVE    WRK-SRYKA       TO  SRH-1005-SRYKA
               INITIALIZE              TERRINF-AREA
               MOVE    "6"             TO  TERRINF-CHKKBN
               MOVE    CHKSND-SRYCD    TO  TERRINF-SRYCD
               MOVE    WRK-LASTYMD     TO  TERRINF-LASTYMD
               MOVE    SDTCHK01-O-TEKBYOMEI (IDX0)
                                       TO  TERRINF-TEKBYOMEI
               MOVE    SDTCHK01-O-TEKCOMPKBN(IDX0)
                                       TO  TERRINF-TEKCOMPKBN
               PERFORM 213-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (15) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (15)
               END-IF
      *
           END-PERFORM
      *
           .
       200112-KNKBYO-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    併用算定の診療行為チェック処理
      *****************************************************************
       200112-HEI-SRY-CHK-SEC          SECTION.
      *
           PERFORM 2001121-SET-FSANTEIYMD-SEC
      *
           INITIALIZE                  THEIYOU-AREA
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   TSRYCD-MAX )
                    OR   ( FLG-CHKEND (11)     =   1 )
      *
               IF    ( TSRYCD-SRYCD (IDX6)(1:1)    NOT =   "6" )
      *
      *            チェックマスタよりチェック
                   IF    ( FLG-CHKEND (11)     =   ZERO )
                       MOVE    "5"         TO  WRK-CHK-CHKKBN
                       MOVE    "2"         TO  WRK-CHK-CDKBN
                       MOVE    TSRYCD-SRYCD (IDX6)
                                           TO  WRK-CHK-SRYCD
                       PERFORM 900-DTCHK008-READ-SEC
                       IF    ( FLG-DTCHK008    =   1 )
                           PERFORM 200112-CHKMST-HEI-SRY-CHK-SEC
                       END-IF
                   END-IF
      *
                   IF    ( FLG-CHKEND (11)     =   ZERO )
                       MOVE    "A"         TO  WRK-CHK-CHKKBN
                       MOVE    "2"         TO  WRK-CHK-CDKBN
                       MOVE    TSRYCD-SRYCD (IDX6)
                                           TO  WRK-CHK-SRYCD
                       PERFORM 900-DTCHK008-READ-SEC
                       IF    ( FLG-DTCHK008    =   1 )
                           PERFORM 200112-CHKMST-HEI-SRY-CHK-SEC
                       END-IF
                   END-IF
      *
                   IF    ( FLG-CHKEND (11)     =   ZERO )
                       MOVE    "B"         TO  WRK-CHK-CHKKBN
                       MOVE    "2"         TO  WRK-CHK-CDKBN
                       MOVE    TSRYCD-SRYCD (IDX6)
                                           TO  WRK-CHK-SRYCD
                       PERFORM 900-DTCHK008-READ-SEC
                       IF    ( FLG-DTCHK008    =   1 )
                           PERFORM 200112-CHKMST-HEI-SRY-CHK-SEC
                       END-IF
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-CHKEND (11)     =   ZERO )
               PERFORM 2001121-CHKMST-KNRHEI-SRY-SEC
           END-IF
      *
      *    エラーメッセージ編集処理
           IF    ( THEIYOU-MAX         >   ZERO )
               INITIALIZE              TERRINF-AREA
               PERFORM 200112-HEI-SRY-ERRMES-HEN-SEC
           END-IF
      *
           .
       200112-HEI-SRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為初回算定日設定処理
      *****************************************************************
       2001121-SET-FSANTEIYMD-SEC      SECTION.
      *
           OPEN    INPUT   DCSANTEI-FILE
      *
           PERFORM 900-DCSANTEI-START-SEC
      *
           PERFORM UNTIL ( FLG-DCSANTEI    NOT =   ZERO )
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   TSRYCD-MAX )
      *
                   IF    ( DCSANTEI-SRYCD  =   TSRYCD-SRYCD (IDX0))
      *
                       MOVE    DCSANTEI-FSANTEIYMD
                                       TO  TSRYCD-FSANTEIYMD(IDX0)
      *
                   END-IF
               END-PERFORM
      *
               PERFORM 900-DCSANTEI-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   DCSANTEI-FILE
      *
           .
       2001121-SET-FSANTEIYMD-EXT.
           EXIT.
      *****************************************************************
      *    併用算定の診療行為チェック処理（チェックマスタ）
      *****************************************************************
       200112-CHKMST-HEI-SRY-CHK-SEC   SECTION.
      *
           MOVE    WRK-CHK-CHKKBN      TO  TCHK-CHK-CHKKBN
           MOVE    TSRYCD-SRYCD (IDX6) TO  TCHK-CHK-SRYCD
           MOVE    WRK-KJNYMD          TO  TCHK-CHK-YMD
           MOVE    WRK-CHK-CDKBN       TO  TCHK-CHK-CDKBN
           PERFORM 230-CHKMST-GET-SEC
           IF    ( TCHK-ACT-CD-MAX     >   ZERO )
               CONTINUE
           ELSE
               MOVE    TSRYCD-SRYCD (IDX6)
                                       TO  WRK-CHK-SRYCD
               PERFORM 900-DTCHK008-WRITE-SEC
           END-IF
      *
      *    チェックマスタより診療行為チェック処理
           PERFORM VARYING IDX10   FROM    1   BY  1
                   UNTIL ( IDX10           >   TCHK-ACT-CD-MAX )
                      OR ( FLG-CHKEND (11) =  1 )
      *
               MOVE    TCHK-ACT-CHKKBN     TO  WRK-CHKKBN
               MOVE    IDX6                TO  IDX-TSRYCD
               MOVE    TCHK-ACT-CD (IDX10) TO  WRK-CHK-CD
               MOVE    TCHK-ACT-NYUGAIBNDKBN (IDX10) 
                                           TO  WRK-NYUGAIBNDKBN
               MOVE    ORCBSD01-CHKKBN (11)
                                           TO  WRK-ORCBSD01-CHKKBN
               MOVE    TCHK-ACT-SYORIKBN (IDX10) 
                                           TO  WRK-SYORIKBN
               MOVE    FLG-CHKEND (11)     TO  WRK-FLG-CHKEND
               IF   (  TCHK-ACT-CHKERR (IDX10) =   8 )
                OR  (( TCHK-ACT-CHKERR (IDX10) =   9 )
                 AND ( SYS-1101-SRYHEIWRNKBN   =   "2" ))
                   CONTINUE
               ELSE
                   PERFORM 200112-CHKMST-SRY-CHK-SEC
               END-IF
               MOVE    WRK-FLG-CHKEND      TO  FLG-CHKEND (11)
      *
           END-PERFORM
      *
           .
       200112-CHKMST-HEI-SRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用算定の診療行為チェック処理（リハ管理料）
      *****************************************************************
       2001121-CHKMST-KNRHEI-SRY-SEC   SECTION.
      *
           PERFORM VARYING IDX-SRY FROM    1   BY  1
                   UNTIL ( IDX-SRY >   TSRYCD-MAX )
                    OR   ( FLG-CHKEND (11)     =   1 )
      *
               PERFORM VARYING IDX-KANRIRYO
                       FROM    1   BY  1
                       UNTIL ( IDX-KANRIRYO > TSRYCD-MAX)
                        OR   ( FLG-CHKEND (11)     =   1 )
      *
                   INITIALIZE  WRK-NISSU-AREA
                   EVALUATE    TRUE
                   WHEN  ( TSRYCD-TYPE (IDX-SRY)       =   "CM"
                                                        OR "CR" )
                    AND  ( TSRYCD-TYPE (IDX-KANRIRYO)(1:1)
                                                       =   "R" )
      *
                   WHEN  ( TSRYCD-TYPE (IDX-SRY)       =   "CS" )
                    AND  ( TSRYCD-TYPE (IDX-KANRIRYO)  =   "RS" )
      *
                   WHEN  ( TSRYCD-TYPE (IDX-SRY)       =   "CN" )
                    AND  ( TSRYCD-TYPE (IDX-KANRIRYO)  =   "RN" )
      *
                   WHEN  ( TSRYCD-TYPE (IDX-SRY)       =   "CU" )
                    AND  ( TSRYCD-TYPE (IDX-KANRIRYO)  =   "RU" )
      *
                   WHEN  ( TSRYCD-TYPE (IDX-SRY)       =   "CK" )
                    AND  ( TSRYCD-TYPE (IDX-KANRIRYO)  =   "RK" )
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
                           IF  ( TSRYCD-DAY-IHOKBN(IDX-SRY IDX-DAY)
                                                   =   "1" )
                              MOVE ORCBSD01-SRYYM TO  WRK-YMD
                              MOVE IDX-DAY        TO  WRK-DD
                              IF ( WRK-YMD >=  TSRYCD-FSANTEIYMD
                                                    (IDX-KANRIRYO))
                                  MOVE  1 TO WRK-NISSU-DAY (IDX-DAY)
                              END-IF
                           END-IF
                       END-PERFORM
                   END-EVALUATE
      *
                   IF    ( WRK-NISSU-DAY-G     NOT =   ZERO )
                       IF    (( TSRYCD-NYUGAIKBN(IDX-SRY)
                                       =   ORCBSD01-NYUGAIKBN )
                          AND ( TSRYCD-HKNFLG   (IDX-SRY)
                                       =   1 ))
                        OR   (( TSRYCD-NYUGAIKBN (IDX-KANRIRYO)
                                       =   ORCBSD01-NYUGAIKBN )
                          AND ( TSRYCD-HKNFLG   (IDX-KANRIRYO)
                                       =   1 ))
                           IF    ( CONST-THEIYOU-MAX   > THEIYOU-MAX )
                               COMPUTE THEIYOU-MAX =   THEIYOU-MAX +   1
      *
                               MOVE    TSRYCD-NYUGAIKBN (IDX-SRY)
                                       TO  THEIYOU-NYUGAIKBN1
                                                         (THEIYOU-MAX)
                               MOVE    TSRYCD-SRYCD (IDX-SRY)
                                       TO  THEIYOU-SRYCD1(THEIYOU-MAX)
                               MOVE    WRK-NISSU-DAY-G
                                       TO  THEIYOU-DAY1-G (THEIYOU-MAX)
      *
                               MOVE    TSRYCD-NYUGAIKBN (IDX-KANRIRYO)
                                       TO  THEIYOU-NYUGAIKBN2
                                                         (THEIYOU-MAX)
                               MOVE    TSRYCD-SRYCD     (IDX-KANRIRYO)
                                       TO  THEIYOU-SRYCD2(THEIYOU-MAX)
                               MOVE    TSRYCD-DAY-G     (IDX-KANRIRYO)
                                       TO  THEIYOU-DAY2-G (THEIYOU-MAX)
      *
                           END-IF
                       END-IF
      *
                       IF    ( ORCBSD01-CHKKBN (11)    =   "1" )
                           MOVE    1   TO  FLG-CHKEND (11)
                       END-IF
      *
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           .
       2001121-CHKMST-KNRHEI-SRY-EXT.
           EXIT.
      *****************************************************************
      *    併用算定の診療行為エラーメッセージ編集処理
      *****************************************************************
       200112-HEI-SRY-ERRMES-HEN-SEC   SECTION.
      *
      *    処理パラメータとは反対の入外を編集
           IF    ( ORCBSD01-NYUGAIKBN   =  "1" )
               MOVE    "（外来）"  TO   WRK-NYUGAI-MES
           ELSE
               MOVE    "（入院）"  TO   WRK-NYUGAI-MES
           END-IF
      *
           IF    ( ORCBSD01-CHKKBN(11)  =  "1" )
               MOVE    1           TO   WRK-MAX
           ELSE
               MOVE    THEIYOU-MAX TO   WRK-MAX
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >    WRK-MAX )
      *
               MOVE    SPACE       TO   WRK-ERRMES-PLUS
               MOVE    SPACE       TO   WRK-LASTYMD
               MOVE    1           TO   WRK-PNT
      *
               IF    ( THEIYOU-CHKKBN (IDX0)   =   "5" )
                   IF    ( THEIYOU-NYUGAIKBN1 (IDX0)
                                   NOT =    ORCBSD01-NYUGAIKBN )
                       STRING  WRK-NYUGAI-MES  DELIMITED   BY  SPACE
                       INTO    WRK-ERRMES-PLUS
                       WITH    POINTER    WRK-PNT
                       END-STRING
                   END-IF
      *
                   MOVE    THEIYOU-SRYCD1 (IDX0)
                                       TO  WRK-SRYCD
                   MOVE    WRK-KJNYMD  TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       NOT =   ZERO )
                       MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
                       PERFORM 800-ZENKAKU-HEN-SEC
                       MOVE    WRK-OUT-INPUT   TO  TNS-NAME
                   END-IF
      *
                   INITIALIZE  WRK-NISSU-AREA
                   MOVE    THEIYOU-DAY1-G (IDX0)
                                           TO  WRK-NISSU-DAY-G
                   PERFORM 200111-NISSU-HEN-SEC
                   MOVE    WRK-NISSU-LASTYMD
                                           TO  WRK-LASTYMD
      *
                   STRING TNS-NAME         DELIMITED   BY  SPACE
                          WRK-NISSU-MES    DELIMITED   BY SPACE
                          "、"             DELIMITED   BY  SIZE
                   INTO   WRK-ERRMES-PLUS
                   WITH    POINTER    WRK-PNT
                   END-STRING
      *
                   IF    ( THEIYOU-NYUGAIKBN2 (IDX0)
                                   NOT =    ORCBSD01-NYUGAIKBN )
                       STRING  WRK-NYUGAI-MES  DELIMITED   BY  SPACE
                       INTO    WRK-ERRMES-PLUS
                       WITH    POINTER    WRK-PNT
                       END-STRING
                   END-IF
      *
                   MOVE    THEIYOU-SRYCD2 (IDX0)
                                       TO  WRK-SRYCD
                   MOVE    WRK-KJNYMD  TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       NOT =   ZERO )
                       MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
                       PERFORM 800-ZENKAKU-HEN-SEC
                       MOVE    WRK-OUT-INPUT   TO  TNS-NAME
                   END-IF
      *
                   INITIALIZE  WRK-NISSU-AREA
                   MOVE    THEIYOU-DAY2-G (IDX0)
                                           TO  WRK-NISSU-DAY-G
                   PERFORM 200111-NISSU-HEN-SEC
      *
                   STRING TNS-NAME         DELIMITED   BY  SPACE
                          WRK-NISSU-MES    DELIMITED   BY SPACE
                   INTO   WRK-ERRMES-PLUS
                   WITH    POINTER    WRK-PNT
                   END-STRING
      *
                   MOVE    "110001"    TO   WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   MOVE    "5"         TO  TERRINF-CHKKBN
                   MOVE    THEIYOU-SRYCD1 (IDX0)
                                       TO  TERRINF-SRYCD
                   MOVE    THEIYOU-SRYCD2 (IDX0)
                                       TO  TERRINF-TEKCD
                   MOVE    WRK-LASTYMD TO  TERRINF-LASTYMD
                   PERFORM 215-ERRMES-HEN-SEC
               ELSE
      *
                   MOVE    THEIYOU-SRYCD1 (IDX0)
                                       TO  WRK-SRYCD
                   MOVE    WRK-KJNYMD  TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       NOT =   ZERO )
                       MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
                       PERFORM 800-ZENKAKU-HEN-SEC
                       MOVE    WRK-OUT-INPUT   TO  TNS-NAME
                   END-IF
      *
                   STRING TNS-NAME         DELIMITED   BY  SPACE
                          "、"             DELIMITED   BY  SIZE
                   INTO   WRK-ERRMES-PLUS
                   WITH    POINTER    WRK-PNT
                   END-STRING
      *
                   MOVE    THEIYOU-SRYCD2 (IDX0)
                                       TO  WRK-SRYCD
                   MOVE    WRK-KJNYMD  TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       NOT =   ZERO )
                       MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
                       PERFORM 800-ZENKAKU-HEN-SEC
                       MOVE    WRK-OUT-INPUT   TO  TNS-NAME
                   END-IF
      *
                   INITIALIZE  WRK-NISSU-AREA
                   MOVE    THEIYOU-DAY1-G (IDX0)
                                           TO  WRK-NISSU-DAY-G
                   PERFORM 200111-NISSU-HEN-SEC
                   MOVE    WRK-NISSU-LASTYMD
                                           TO  WRK-LASTYMD
      *
                   STRING TNS-NAME         DELIMITED   BY  SPACE
                          WRK-NISSU-MES    DELIMITED   BY SPACE
                   INTO   WRK-ERRMES-PLUS
                   WITH    POINTER    WRK-PNT
                   END-STRING
      *
                   IF    ( THEIYOU-CHKKBN (IDX0)   =   "A" )
                       MOVE    "110002"    TO   WRK-ERRCD
                   ELSE
                       MOVE    "110003"    TO   WRK-ERRCD
                   END-IF
                   INITIALIZE              TERRINF-AREA
                   MOVE    THEIYOU-CHKKBN (IDX0)
                                       TO  TERRINF-CHKKBN
                   MOVE    THEIYOU-SRYCD1 (IDX0)
                                       TO  TERRINF-SRYCD
                   MOVE    THEIYOU-SRYCD2 (IDX0)
                                       TO  TERRINF-TEKCD
                   MOVE    WRK-LASTYMD TO  TERRINF-LASTYMD
                   PERFORM 215-ERRMES-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       200112-HEI-SRY-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為どうしの算定漏れチェック
      *****************************************************************
       200111-SRYSNT-CHK-SEC           SECTION.
      *
           IF    ( ORCBSD01-CHKKBN(16) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (16)
           END-IF
      *
      *    併用算定の診療行為チェック処理
           IF    ( FLG-CHKEND (16)     =   ZERO )
            AND  ( FLG-SRYSNT          =   1 )
               PERFORM 200112-SRYSNT-CHK-SEC
           END-IF
      *
           .
       200111-SRYSNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為どうしの算定漏れチェック
      *****************************************************************
       200112-SRYSNT-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   TSRYCD-MAX )
                    OR   ( FLG-CHKEND (16)     =   1 )
      *
      *        チェックマスタよりチェック
               IF    ( FLG-CHKEND (16)     =   ZERO )
                   MOVE    "16"        TO  WRK-REIGAI-CHKNUM
                   MOVE    "7"         TO  WRK-REIGAI-CHKKBN
                   MOVE    TSRYCD-SRYCD (IDX6)
                                       TO  WRK-REIGAI-CD
                   PERFORM 800-REIGAI-CHK-SEC
                   IF    ( FLG-REIGAI  =   ZERO )
                       MOVE    "7"         TO  WRK-CHK-CHKKBN
                       MOVE    "2"         TO  WRK-CHK-CDKBN
                       MOVE    TSRYCD-SRYCD (IDX6)
                                           TO  WRK-CHK-SRYCD
                       PERFORM 900-DTCHK008-READ-SEC
                       IF    ( FLG-DTCHK008    =   1 )
                           PERFORM 200112-CHKMST-SRYSNT-CHK-SEC
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       200112-SRYSNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為どうしの算定漏れチェック処理（チェックマスタ）
      *****************************************************************
       200112-CHKMST-SRYSNT-CHK-SEC   SECTION.
      *
           INITIALIZE                  FLG-SRYSNT-G
      *
           MOVE    "7"                 TO  TCHK-CHK-CHKKBN
           MOVE    TSRYCD-SRYCD (IDX6) TO  TCHK-CHK-SRYCD
           MOVE    WRK-KJNYMD          TO  TCHK-CHK-YMD
           MOVE    "2"                 TO  TCHK-CHK-CDKBN
           PERFORM 230-CHKTRD-GET-SEC
           IF    ( TCHK-ACT-CD-MAX     =   ZERO )
               IF    ( SYS-1101-SRYMATERKKBN   =   "1" )
                   PERFORM 231-CHKTRD-GET-SEC
               END-IF
           END-IF
           IF    ( TCHK-ACT-CD-MAX     =   ZERO )
               MOVE    "7"             TO  WRK-CHK-CHKKBN
               MOVE    "2"             TO  WRK-CHK-CDKBN
               MOVE    TSRYCD-SRYCD (IDX6)
                                       TO  WRK-CHK-SRYCD
               PERFORM 900-DTCHK008-WRITE-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-TARGET
      *
           IF    ( TCHK-ACT-NYUGAIKBN (1)  =   SPACE )
              OR ( TCHK-ACT-NYUGAIKBN (1)  =   ORCBSD01-NYUGAIKBN )
               CONTINUE
           ELSE
               MOVE    ZERO            TO  FLG-TARGET
           END-IF
      *
           IF    ( TSRYCD-NYUGAIKBN (IDX6)     =   ORCBSD01-NYUGAIKBN )
               CONTINUE
           ELSE
               MOVE    ZERO            TO  FLG-TARGET
           END-IF
      *
           IF    ( FLG-TARGET          =   1 )
      *
               PERFORM VARYING IDX10   FROM    1   BY  1
                       UNTIL ( IDX10   >   TCHK-ACT-CD-MAX )
                   IF    ( TCHK-ACT-DAYMONTHKBN (IDX10)
                                       =   ZERO )
                       MOVE    1       TO  FLG-SRYSNT-DAYCHK
                   ELSE
                       MOVE    1       TO  FLG-SRYSNT-MONTHCHK
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-SRYSNT-DAYCHK   =   1 )
                   PERFORM VARYING IDX10   FROM    1   BY  1
                       UNTIL ( IDX10   >   31 )
                       IF   ( TSRYCD-DAY ( IDX6 IDX10)    >   ZERO )
                       AND  ( TSRYCD-DAY-NYUGAIKBN (IDX6 IDX10)
                                           =   ORCBSD01-NYUGAIKBN )
                       AND  ( TSRYCD-DAY-HKNFLG   (IDX6 IDX10)
                                                          =   1 )
                           PERFORM 200112-CHKTRD-DAY-CHK-SEC
                       END-IF
                   END-PERFORM
      *
                   INITIALIZE  WRK-NISSU-AREA
                   PERFORM VARYING IDX10   FROM    1   BY  1
                           UNTIL ( IDX10   >   31 )
                       IF    ( FLG-SRYSNT-DAYERR (IDX10)   =   1 )
                           MOVE    1       TO  WRK-NISSU-DAY (IDX10)
                       END-IF
                   END-PERFORM
      *
                   IF    ( WRK-NISSU-DAY-G NOT =   ZERO )
                       MOVE    "160001"    TO  WRK-ERRCD
                       PERFORM 200112-SRYSNT-ERR-HEN-SEC
                   END-IF
      *
               END-IF
      *
               IF    ( FLG-SRYSNT-MONTHCHK =   1 )
                   PERFORM 200112-CHKTRD-MONTH-CHK-SEC
                   IF    ( FLG-SRYSNT-MONTHERR =   1 )
                       INITIALIZE              WRK-NISSU-DAY-G
                       PERFORM VARYING IDX10   FROM    1   BY  1
                               UNTIL ( IDX10   >   31 )
                           IF    ( TSRYCD-DAY-HKNFLG (IDX6 IDX10)
                                               =   1 )
                             IF  ( TSRYCD-DAY-NYUGAIKBN (IDX6 IDX10)
                                               =   ORCBSD01-NYUGAIKBN )
                                 MOVE  1       TO  WRK-NISSU-DAY (IDX10)
                             END-IF
                           END-IF
                       END-PERFORM
                       IF    ( WRK-NISSU-DAY-G NOT =   ZERO )
                           MOVE    "160002"    TO  WRK-ERRCD
                           PERFORM 200112-SRYSNT-ERR-HEN-SEC
                       END-IF
                   END-IF
               END-IF
      *
           END-IF
      *
      *
           .
       200112-CHKMST-SRYSNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３診療行為チェック処理（日別）
      *****************************************************************
       200112-CHKTRD-DAY-CHK-SEC       SECTION.
      *
           PERFORM VARYING IDX31   FROM    1   BY  1
                   UNTIL ( IDX31           >   TCHK-ACT-CD-MAX )
                    OR   ( FLG-SRYSNT-DAYERR (IDX10)   =  2 )
      *
               IF    ( TCHK-ACT-DAYMONTHKBN (IDX31)
                                               =   ZERO )
                   MOVE    1       TO  FLG-SRYSNT-DAYERR (IDX10)
      *
                   PERFORM VARYING IDX32   FROM   1    BY  1
                           UNTIL ( IDX32   >   TSRYCD-MAX )
                       IF    ( TCHK-ACT-CD (IDX31)
                                           =   TSRYCD-SRYCD (IDX32))
                        AND  ( TSRYCD-DAY (IDX32 IDX10)    >   ZERO )
      *
                           IF    ( TCHK-ACT-NYUGAIBNDKBN (IDX31)
                                                           =   "2" )
                               IF    ( TSRYCD-DAY-NYUGAIKBN
                                                      (IDX32 IDX10)
                                           =   ORCBSD01-NYUGAIKBN )
                                   MOVE    2
                                       TO FLG-SRYSNT-DAYERR (IDX10)
                               END-IF
                           ELSE
                                   MOVE    2
                                       TO FLG-SRYSNT-DAYERR (IDX10)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       200112-CHKTRD-DAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３診療行為チェック処理（月別）
      *****************************************************************
       200112-CHKTRD-MONTH-CHK-SEC     SECTION.
      *
           PERFORM VARYING IDX31   FROM    1   BY  1
                   UNTIL ( IDX31           >   TCHK-ACT-CD-MAX )
                    OR   ( FLG-SRYSNT-MONTHERR =   2 )
      *
               IF    ( TCHK-ACT-DAYMONTHKBN (IDX31)
                                               =   "1" )
                   MOVE    1       TO  FLG-SRYSNT-MONTHERR
      *
                   PERFORM VARYING IDX32   FROM   1    BY  1
                           UNTIL ( IDX32      >   TSRYCD-MAX )
                       IF    ( TCHK-ACT-CD (IDX31)
                                              =   TSRYCD-SRYCD (IDX32))
      *
                           IF    ( TCHK-ACT-NYUGAIBNDKBN(IDX31)
                                                           =   "2" )
                               IF    ( TSRYCD-NYUGAIKBN (IDX32)
                                               =   ORCBSD01-NYUGAIKBN )
                                   MOVE    2   TO FLG-SRYSNT-MONTHERR
                               END-IF
                           ELSE
                                   MOVE    2   TO FLG-SRYSNT-MONTHERR
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       200112-CHKTRD-MONTH-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為どうしの算定漏れチェックエラー編集処理
      *****************************************************************
       200112-SRYSNT-ERR-HEN-SEC       SECTION.
      *
           MOVE    TSRYCD-SRYCD (IDX6) TO  WRK-SRYCD
           MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  TNS-NAME
           END-IF
      *
           PERFORM 200111-NISSU-HEN-SEC
           MOVE    WRK-NISSU-LASTYMD
                               TO   WRK-LASTYMD
      *
           MOVE    SPACE       TO   WRK-ERRMES-PLUS
      *
           STRING TNS-NAME         DELIMITED   BY  SPACE
                  WRK-NISSU-MES    DELIMITED   BY SPACE
           INTO   WRK-ERRMES-PLUS
           END-STRING
      *
           INITIALIZE              TERRINF-AREA
           MOVE    "7"         TO  TERRINF-CHKKBN
           MOVE    TSRYCD-SRYCD (IDX6)
                               TO  TERRINF-SRYCD
           MOVE    WRK-LASTYMD TO  TERRINF-LASTYMD
           PERFORM 215-ERRMES-HEN-SEC
           IF    ( ORCBSD01-CHKKBN (16)    =   "1" )
               MOVE    1       TO  FLG-CHKEND (16)
           END-IF
      *
           .
       200112-SRYSNT-ERR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病名と薬剤
      *****************************************************************
       200111-BYOYKZ-CHK-SEC           SECTION.
      *
           IF    ( ORCBSD01-CHKKBN(17) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (17)
           END-IF
      *
      *    病名と薬剤チェック処理
           IF    ( FLG-CHKEND (17)     =   ZERO )
            AND  ( FLG-BYOYKZ          =   1 )
               MOVE    "8"             TO  WRK-CHKKBN
               MOVE    "2"             TO  WRK-CDKBN
               MOVE    17              TO  IDX-CHK
               PERFORM 200112-CHK005-CHK-SEC
           END-IF
      *
           .
       200111-BYOYKZ-EXT.
           EXIT.
      *****************************************************************
      *    病名と診療行為
      *****************************************************************
       200111-BYOSRY-CHK-SEC           SECTION.
      *
           IF    ( ORCBSD01-CHKKBN(18) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (18)
           END-IF
      *
      *    病名と診療行為チェック処理
           IF    ( FLG-CHKEND (18)     =   ZERO )
            AND  ( FLG-BYOSRY          =   1 )
               MOVE    "9"             TO  WRK-CHKKBN
               MOVE    "2"             TO  WRK-CDKBN
               MOVE    18              TO  IDX-CHK
               PERFORM 200112-CHK005-CHK-SEC
           END-IF
      *
           .
       200111-BYOSRY-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５チェック処理
      *****************************************************************
       200112-CHK005-CHK-SEC           SECTION.
      *
           INITIALIZE                  SDTCHK06AREA
           IF    ( SYS-1101-BYOCHKPTN-G    =   SPACE )
               MOVE    CONST-CHKPTN    TO  SDTCHK06-CHKPTN-G
           ELSE
               MOVE    SYS-1101-BYOCHKPTN-G
                                       TO  SDTCHK06-CHKPTN-G
           END-IF
           MOVE    DTCHK003-HOSPNUM TO  SDTCHK06-HOSPNUM
           MOVE    DTCHK003-PTID   TO  SDTCHK06-PTID
           MOVE    WRK-CHKKBN      TO  SDTCHK06-CHKKBN
           MOVE    WRK-KJNYMD      TO  SDTCHK06-KJNYMD
           MOVE    WRK-CDKBN       TO  SDTCHK06-CDKBN
           MOVE    ORCBSD01-NYUGAIKBN
                                   TO  SDTCHK06-NYUGAIKBN
           MOVE    DTCHK003-HKNKBN TO  SDTCHK06-HKNKBN
           CALL    "ORCSDTCHK06"   USING   SDTCHK06AREA
                                           SYS-1101-REC
                                           SDTCHK02AREA
                                           LNK-HKNCOMBI-AREA
                                           SPA-AREA
                                           DTCHKCOMMON-AREA
      *
           PERFORM VARYING IDX33   FROM    1   BY  1
                   UNTIL ( IDX33   >   SDTCHK06-O-MAX )
                    OR   ( FLG-CHKEND (IDX-CHK)    =   1 )
               PERFORM 2001121-CHK005-CHK-SEC
           END-PERFORM
      *
           .
       200112-CHK005-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５チェック処理
      *****************************************************************
       2001121-CHK005-CHK-SEC          SECTION.
      *
           MOVE    WRK-CHKKBN          TO  TCHK-CHK-CHKKBN
           MOVE    SDTCHK06-O-ID (IDX33)
                                       TO  TCHK-CHK-ID
           PERFORM 230-CHK005-GET-SEC
           IF    ( TCHK-ACT-CD-MAX     >   ZERO )
               MOVE    IDX-CHK         TO  WRK-REIGAI-CHKNUM9
               MOVE    WRK-CHKKBN      TO  WRK-REIGAI-CHKKBN
               MOVE    TCHK-ACT-BYOMEI
                                       TO  WRK-REIGAI-CD
               PERFORM 800-REIGAI-CHK-SEC
               IF    ( FLG-REIGAI  =   ZERO )
                   PERFORM 20011211-CHK005-CHK-SEC
               END-IF
           END-IF
      *
           .
       2001121-CHK005-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５チェック処理
      *****************************************************************
       20011211-CHK005-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-HIT
      *
           IF    ( SYS-1101-BYOSRYKAKBN =  "1" OR SPACE )
      *
               MOVE    ZERO        TO  FLG-SRYKA
               PERFORM VARYING IDX34   FROM    1   BY  1
                       UNTIL ( IDX34   >   TSRYKA-MAX )
                        OR   ( FLG-SRYKA   =   1 )
                   IF    ( TSRYKA-SRYKA (IDX34)
                                       =   SDTCHK06-O-SRYKA(IDX33))
                       MOVE    1       TO  FLG-SRYKA
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-SRYKA       =   ZERO )
                       MOVE    1       TO  FLG-HIT
               END-IF
      *
           END-IF
      *
           PERFORM VARYING IDX31   FROM    1   BY  1
                   UNTIL ( IDX31   >  TCHK-ACT-CD-MAX )
                    OR   ( FLG-HIT =   1 )
             PERFORM VARYING IDX34   FROM    1   BY  1
                     UNTIL ( IDX34   >   TSRYKA-MAX )
                      OR   ( FLG-HIT =   1 )
               PERFORM VARYING IDX7    FROM    1   BY  1
                       UNTIL ( IDX7    >   TSRYKA-SRYCD-MAX(IDX34))
                        OR   ( FLG-HIT =   1 )
      *
                 IF    ( TCHK-ACT-CD (IDX31)
                                   =   TSRYKA-SRYCD (IDX34 IDX7))
                   PERFORM VARYING IDX-DAY FROM  1  BY 1
                           UNTIL ( IDX-DAY >   31 )
                            OR   ( FLG-HIT =   1 )
      *
                     IF    ( TSRYKA-SRYCD-DAY (IDX34 IDX7 IDX-DAY)
                                                   > ZERO )
                      AND  ( SDTCHK06-O-BYODAY (IDX33 IDX-DAY)
                                                   > ZERO)
                       IF    ( SYS-1101-BYOSRYKAKBN
                                       =  "1" OR SPACE )
                           IF    ( TSRYKA-SRYKA (IDX34)
                                       =   SDTCHK06-O-SRYKA(IDX33))
                               MOVE    1   TO  FLG-HIT
                           END-IF
                       ELSE
                                   MOVE    1   TO  FLG-HIT
                       END-IF
                     END-IF
                   END-PERFORM
                 END-IF
               END-PERFORM
             END-PERFORM
           END-PERFORM
      *
               IF    ( FLG-HIT         =   ZERO )
                   MOVE    SDTCHK06-O-PTBYOMEI (IDX33)
                                               TO  WRK-PTBYOMEI
                   MOVE    SDTCHK06-O-SRYYMD   (IDX33)
                                               TO  WRK-PTBYOSRYYMD
                   PERFORM 2001121-PTBYOMES-PLUS-SEC
                   MOVE    SDTCHK06-O-SRYKA (IDX33)
                                               TO  SRH-1005-SRYKA
                   MOVE    WRK-PTBYOMES-PLUS   TO  WRK-ERRMES-PLUS
                   IF    ( WRK-CHKKBN      =   "8" )
                       MOVE    "170001"        TO  WRK-ERRCD
                   ELSE
                       MOVE    "180001"        TO  WRK-ERRCD
                   END-IF
                   INITIALIZE              TERRINF-AREA
                   MOVE    WRK-CHKKBN          TO  TERRINF-CHKKBN
                   MOVE    TCHK-ACT-BYOMEI     TO  TERRINF-BYOMEI
                   PERFORM 213-ERRMES-HEN-SEC
                   IF    ( ORCBSD01-CHKKBN (IDX-CHK) =   "1" )
                       MOVE    1               TO  FLG-CHKEND (IDX-CHK)
                   END-IF
               END-IF
      *
           .
       20011211-CHK005-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実日数算定チェック処理
      *****************************************************************
       200111-JNISSU-CHK-SEC           SECTION.
      *
           IF    ( ORCBSD01-CHKKBN(12) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (12)
           END-IF
      *
           IF    ( FLG-CHKEND (12)     =   ZERO )
      *
      *        実日数退避ワーク日数カウント
               PERFORM 200112-TJNISSU-DAY-CNT-SEC
      *
      *        算定回数退避ワーク日数カウント
               PERFORM 200112-TKAISU-DAY-CNT-SEC
      *
      *        実日数退避ワークチェック
               PERFORM 200112-TJNISSU-CHK-SEC
           END-IF
      *
           IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
               IF    ( FLG-CHKEND (12) =   ZERO )
      *            入院診療行為日チェック処理
                   PERFORM 200112-NACCT-SRY-DAY-CHK-SEC
               END-IF
      *
               IF    ( FLG-CHKEND (12) =   ZERO )
      *            入院調剤料算定日チェック処理
                   MOVE    TNYUIN-CHOZAI-DAY-OCC (2)
                                       TO  TNYUIN-CHOZAI-DAY-OCC (1)
                   MOVE    "120003"    TO  WRK-ERRCD
                   PERFORM 200112-NCHOZAI-DAY-CHK-SEC
      *
      *            入院調剤技術基本料算定日チェック処理
                   MOVE    TNYUIN-CHOZAI-DAY-OCC (3)
                                       TO  TNYUIN-CHOZAI-DAY-OCC (1)
                   MOVE    "120005"    TO  WRK-ERRCD
                   PERFORM 200112-NCHOZAI-DAY-CHK-SEC
               END-IF
      *
           END-IF
      *
           .
       200111-JNISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実日数退避ワーク日数カウント処理
      *****************************************************************
       200112-TJNISSU-DAY-CNT-SEC      SECTION.
      *
           PERFORM VARYING IDX13   FROM    1   BY  1
                   UNTIL ( IDX13   >   TJNISSU-MAX )
      *
               MOVE    ZERO        TO  TJNISSU-DAY-CNT (IDX13)
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   31 )
                   IF    ( TJNISSU-DAY (IDX13 IDX0)    =   1 )
                       COMPUTE TJNISSU-DAY-CNT (IDX13)
                           =   TJNISSU-DAY-CNT (IDX13) +   1
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       200112-TJNISSU-DAY-CNT-EXT.
           EXIT.
      *****************************************************************
      *    算定回数退避ワーク日数カウント処理
      *****************************************************************
       200112-TKAISU-DAY-CNT-SEC       SECTION.
      *
           PERFORM VARYING IDX12   FROM    1   BY  1
                   UNTIL ( IDX12   >   TKAISU-MAX )
      *
               PERFORM VARYING IDX14   FROM    1   BY  1
                       UNTIL ( IDX14   >   TKAISU-SRYCD-MAX (IDX12))
      *
                   MOVE    ZERO    TO  TKAISU-SRYCD-CNT (IDX12 IDX14)
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   31 )
      *
                       IF    ( TKAISU-SRYCD-DAY (IDX12 IDX14 IDX0 )
                                       =   1 )
                           COMPUTE TKAISU-SRYCD-CNT (IDX12 IDX14)
                               =   TKAISU-SRYCD-CNT (IDX12 IDX14)  +   1
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM
      *
           .
       200112-TKAISU-DAY-CNT-EXT.
           EXIT.
      *****************************************************************
      *    実日数退避ワークチェック
      *****************************************************************
       200112-TJNISSU-CHK-SEC          SECTION.
      *
           PERFORM VARYING IDX12   FROM    1   BY  1
                   UNTIL ( IDX12   >   TKAISU-MAX )
                    OR   ( FLG-CHKEND (12) =   1 )
      *
               MOVE    ZERO            TO  FLG-JNISSU-HIT
      *
               PERFORM VARYING IDX13   FROM    1   BY  1
                       UNTIL ( IDX13   >   TJNISSU-MAX )
                        OR   ( FLG-CHKEND (12) =   1 )
      *
                   IF    ( TJNISSU-HKNNUM (IDX13)
                                       =   TKAISU-HKNNUM (IDX12) )
                       MOVE    1       TO  FLG-JNISSU-HIT
                       PERFORM 200112-TJNISSU-DAY-CHK-SEC
                       MOVE    TJNISSU-MAX      TO  IDX13
                   END-IF
      *
               END-PERFORM
      *
      *        実日数算定元の剤が存在しない場合で、
      *        診療行為の算定回数が設定されている場合はエラーとする
               IF    ( FLG-JNISSU-HIT  =   ZERO )
                   PERFORM VARYING IDX19   FROM    1   BY  1
                           UNTIL ( IDX19   >   TKAISU-SRYCD-MAX (IDX12))
                            OR   ( FLG-CHKEND (12) =   1 )
      *
                       IF    ( TKAISU-SRYCD-CNT (IDX12 IDX19)
                                               >   ZERO )
      *
                           MOVE    TKAISU-HKNNUM (IDX12)
                                           TO  WRK-HKNNUM
                           MOVE    TKAISU-SRYCD (IDX12 IDX19)
                                           TO  WRK-TKAISU-SRYCD
      *
                           INITIALIZE              TERRINF-AREA
                           PERFORM 200112-JNISSU-ERRMES-HEN-SEC
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX13   FROM    1   BY  1
                   UNTIL ( IDX13   >   TJNISSU-MAX )
                    OR   ( FLG-CHKEND (12) =   1 )
               IF    ( TJNISSU-DAY-CNT (IDX13) =   ZERO )
                   MOVE    TJNISSU-HKNNUM (IDX13)
                                           TO  SRH-HKNNUM
                   MOVE    "120004"        TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 211-ERRMES-HEN-SEC
               END-IF
           END-PERFORM
      *
           .
       200112-TJNISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実日数退避ワークチェック
      *****************************************************************
       200112-TJNISSU-DAY-CHK-SEC      SECTION.
      *
           PERFORM VARYING IDX14   FROM    1   BY  1
                   UNTIL ( IDX14   >   TKAISU-SRYCD-MAX (IDX12))
                    OR   ( FLG-CHKEND (12) =   1 )
      *
               IF    ( TJNISSU-DAY-CNT (IDX13)
                                   <   TKAISU-SRYCD-CNT (IDX12 IDX14))
                   MOVE    TKAISU-HKNNUM (IDX12)
                                       TO  WRK-HKNNUM
                   MOVE    TKAISU-SRYCD (IDX12 IDX14)
                                       TO  WRK-TKAISU-SRYCD
      *
                   INITIALIZE              TERRINF-AREA
                   PERFORM 200112-JNISSU-ERRMES-HEN-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200112-TJNISSU-DAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実日数エラーメッセージ編集処理
      *****************************************************************
       200112-JNISSU-ERRMES-HEN-SEC    SECTION.
      *
           MOVE    WRK-HKNNUM          TO  SRH-HKNNUM
           MOVE    WRK-TKAISU-SRYCD    TO  WRK-SRYCD
           MOVE    WRK-KJNYMD  TO  WRK-TNS-YMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU       NOT =   ZERO )
               MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  TNS-NAME
           END-IF
      *
           MOVE    TNS-NAME    TO  WRK-ERRMES-PLUS
           MOVE    "120001"    TO  WRK-ERRCD
           INITIALIZE              TERRINF-AREA
           PERFORM 216-ERRMES-HEN-SEC
      *
           IF    ( ORCBSD01-CHKKBN (12)    =   "1" )
               MOVE    1       TO  FLG-CHKEND (12)
           END-IF
      *
           .
       200112-JNISSU-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為日チェック処理
      *****************************************************************
       200112-NACCT-SRY-DAY-CHK-SEC    SECTION.
      *
           INITIALIZE          WRK-NISSU-AREA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   31 )
      *
               IF    ( TNACCT-KKN-DAY (IDX0)       =   ZERO )
                   IF    ( TNYUIN-ACCT-DAY (IDX0)  =   1 )
                       MOVE    1       TO  WRK-NISSU-DAY (IDX0)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-NISSU-DAY-G    NOT  =   ZERO )
               PERFORM 200111-NISSU-HEN-SEC
               MOVE    WRK-NISSU-MES   TO   WRK-ERRMES-PLUS
               MOVE    "120002"    TO  WRK-ERRCD
      *
               INITIALIZE              TERRINF-AREA
               PERFORM 215-ERRMES-HEN-SEC
               MOVE    1           TO  FLG-CHKEND (12)
           END-IF
      *
           .
       200112-NACCT-SRY-DAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院調剤料算定日チェック処理
      *****************************************************************
       200112-NCHOZAI-DAY-CHK-SEC      SECTION.
      *
           INITIALIZE                  WRK-NISSU-AREA
           MOVE    ZERO            TO  CNT-GAIHAKU-ERR
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   31 )
      *
               IF    ( TNACCT-GAIHAKU-DAY (IDX0)
                                           =   ZERO OR >= 4 )
                   CONTINUE
               ELSE
                   IF    ( TNYUIN-CHOZAI-DAY (1 IDX0)   =   1 )
                       COMPUTE CNT-GAIHAKU-ERR
                           =   CNT-GAIHAKU-ERR +   1
                       MOVE    1       TO  WRK-NISSU-DAY (IDX0)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( CNT-GAIHAKU-ERR     >   ZERO )
               PERFORM 200111-NISSU-HEN-SEC
               MOVE    WRK-NISSU-MES   TO  WRK-ERRMES-PLUS
      *
               INITIALIZE              TERRINF-AREA
               PERFORM 215-ERRMES-HEN-SEC
               MOVE    1               TO  FLG-CHKEND (12)
           END-IF
      *
           .
       200112-NCHOZAI-DAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者氏名チェック処理
      *****************************************************************
       200111-PTNAME-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-NORMAL
           MOVE    ZERO                TO  FLG-TANKI-SHOBYO
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   THKNCOMBI-MAX )
               MOVE    THKNCOMBI-HKNCOMBI (IDX0)
                                       TO  WRK-HKNCOMBI
               PERFORM 220-HKNCOMBI-GET-SEC
               IF    ( DTCHK003-HKNKBN     =   CONST-ROSAI )
                AND  ( COMB-HKNNUM         =   "971" )
                AND  ( ORCBSD01-SRYYM     >=   CONST-H2507 )
                AND  ( FLG-RSI-RECEDEN-CHK =   1 )
                AND  ( SYS-4001-RSIIRYOKBN =   "1" )
                AND  ( WRK-COMB-RSI-HKNKBN =   "1" OR "2" )
                   MOVE    1           TO  FLG-TANKI-SHOBYO
               ELSE
                   MOVE    1           TO  FLG-NORMAL
               END-IF
           END-PERFORM
      *
           IF    ( ORCBSD01-CHKKBN(13) =   ZERO )
               MOVE    1                   TO  FLG-CHKEND (13)
           END-IF
      *
           IF    ( THKNCOMBI-MAX       =   ZERO )
               MOVE    1                   TO  FLG-CHKEND (13)
           END-IF
      *
      *    氏名チェック
           IF    ( FLG-CHKEND (13)     =   ZERO )
               PERFORM 200111-NAME-CHK-SEC
           END-IF
      *
      *    カナ氏名チェック
           IF    ( FLG-CHKEND (13)     =   ZERO )
               PERFORM 200111-KANANAME-CHK-SEC
           END-IF
      *
           .
       200111-PTNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者氏名チェック処理
      *****************************************************************
       200111-NAME-CHK-SEC             SECTION.
      *
           MOVE    PTINF-NAME          TO  WRK-PTINF-NAME
      *
           INSPECT WRK-PTINF-NAME      REPLACING   ALL "　"    BY  "  "
      *
           IF    ( WRK-PTINF-NAME      =   SPACE )
      *
               MOVE    "130003"        TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 210-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (13) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (13)
                   GO  TO  200111-NAME-CHK-EXT
               END-IF
      *
           END-IF
      *
           IF    ( PTINF-NAME          NOT =   SPACE )
               MOVE    PTINF-NAME      TO  WRK-KANACHK-MAE-INPUT
               PERFORM 801-KANA-CHK-SEC
               IF  ( FLG-KANACHK   NOT =   ZERO )
      *
                   IF    ( FLG-KANACHK =   2 )
                       MOVE    "130005"    TO  WRK-ERRCD
                   ELSE
                       MOVE    "130001"    TO  WRK-ERRCD
                   END-IF
                   INITIALIZE              TERRINF-AREA
                   PERFORM 210-ERRMES-HEN-SEC
               END-IF
      *
               IF    ( ORCBSD01-CHKKBN (13) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (13)
                   GO  TO  200111-NAME-CHK-EXT
               END-IF
           END-IF
      *
           .
       200111-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者カナ氏名チェック処理
      *****************************************************************
       200111-KANANAME-CHK-SEC         SECTION.
      *
           MOVE    PTINF-KANANAME      TO  WRK-PTINF-KANANAME
      *
           INSPECT WRK-PTINF-KANANAME  REPLACING   ALL "　"    BY  "  "
      *
           IF    ( WRK-PTINF-KANANAME  =   SPACE )
      *
               MOVE    "130004"        TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 210-ERRMES-HEN-SEC
      *
               IF    ( ORCBSD01-CHKKBN (13) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (13)
                   GO  TO  200111-KANANAME-CHK-EXT
               END-IF
           ELSE
               IF    ( FLG-NORMAL          =   1 )
                   MOVE    PTINF-KANANAME      TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 800-KATAKANA-CHK-SEC
                   IF  ( FLG-KANACHK   NOT =   ZERO )
                       MOVE    "130002"        TO  WRK-ERRCD
                       INITIALIZE              TERRINF-AREA
                       PERFORM 210-ERRMES-HEN-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-TANKI-SHOBYO    =   1 )
                   MOVE    PTINF-KANANAME      TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 801-KATAKANA-CHK-SEC
                   IF  ( FLG-KANACHK   NOT =   ZERO )
                       MOVE    WRK-ERR-KANA    TO  WRK-ERRMES-PLUS
                       MOVE    "130007"        TO  WRK-ERRCD
                       INITIALIZE              TERRINF-AREA
                       PERFORM 215-ERRMES-HEN-SEC
                   END-IF
               END-IF
      *
               IF    ( ORCBSD01-CHKKBN (13) =   "1" )
                   MOVE    1           TO  FLG-CHKEND (13)
                   GO  TO  200111-KANANAME-CHK-EXT
               END-IF
           END-IF
      *
           .
       200111-KANANAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組み合わせチェック処理
      *****************************************************************
       200111-HKNCOMBI-CHK-SEC         SECTION.
      *
           IF    ( ORCBSD01-CHKKBN(14) =   ZERO )
               MOVE    1                   TO  FLG-CHKEND (14)
           END-IF
      *
           IF    ( THKNCOMBI-MAX       =   ZERO )
               MOVE    1                   TO  FLG-CHKEND (14)
           END-IF
      *
           IF    ( FLG-CHKEND (14)     =   ZERO )
      *        診療会計保険組み合わせチェック
               PERFORM 200111-ACCT-HKNCOMBI-CHK-SEC
           END-IF
      *
           .
       200111-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療会計保険組み合わせチェック処理
      *****************************************************************
       200111-ACCT-HKNCOMBI-CHK-SEC    SECTION.
      *
           PERFORM VARYING IDX16   FROM    1   BY  1
                   UNTIL ( IDX16   >       THKNCOMBI-MAX )
                    OR   ( FLG-CHKEND (14) =   1 )
      *
               MOVE    SPACE               TO  WRK-ACCTNAME
               IF    ( THKNCOMBI-ACCTKBN (IDX16)   =   "1" )
                   MOVE    "診療会計："    TO  WRK-ACCTNAME
               ELSE
                   MOVE    "入院会計："    TO  WRK-ACCTNAME
               END-IF
      *
               PERFORM VARYING IDX17   FROM    1   BY  1
                       UNTIL ( IDX17   >   SDTCHK02-O-MAX )
                        OR  (( THKNCOMBI-HKNCOMBI (IDX16)
                                       =   LNK-COMB-HKNCOMBINUM
                                                              (IDX17))
                         AND ( LNK-COMB-DELKBN (IDX17) NOT =   "1" ))
                   CONTINUE
      *
               END-PERFORM
      *
               IF    ( IDX17   >   SDTCHK02-O-MAX )
                   MOVE    THKNCOMBI-HKNCOMBI (IDX16)
                                           TO  WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    WRK-OUT-INPUT   TO  WRK-HKNCOMBIMEI
                   MOVE    THKNCOMBI-DAY-G (IDX16)
                                       TO  WRK-NISSU-DAY-G
                   PERFORM 200111-NISSU-HEN-SEC
                   MOVE    SPACE       TO  WRK-ERRMES-PLUS
                   STRING  WRK-ACCTNAME    DELIMITED   BY  SPACE
                           WRK-HKNCOMBIMEI DELIMITED   BY  SPACE
                           WRK-NISSU-MES   DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
      *
                   MOVE    "140001"    TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 215-ERRMES-HEN-SEC
                   IF    ( ORCBSD01-CHKKBN (14)    =   "1" )
                       MOVE    1       TO  FLG-CHKEND (14)
                   END-IF
               ELSE
      *
                   PERFORM 2001111-ACCT-HKNCOMBI-CHK-SEC
      *
      *            地方公費単独チェック
                   IF    ( FLG-CHKEND (14)     =   ZERO )
                       IF    ( LNK-COMB-HKNNUM (IDX17) =   SPACE )
                           PERFORM 2001111-CHIHOU-KOH-CHK-SEC
                       END-IF
                   END-IF
      *
      *            公費の種類チェック処理
                   IF    ( FLG-CHKEND (14)     =   ZERO )
                           PERFORM 2001111-HKNKOHNUM-CHK-SEC
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-ACCT-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療会計保険組み合わせチェック処理
      *****************************************************************
       2001111-ACCT-HKNCOMBI-CHK-SEC   SECTION.
      *
           IF    ( THKNCOMBI-STYMD (IDX16)
                               >=  LNK-COMB-TEKSTYMD (IDX17))
            AND  ( THKNCOMBI-EDYMD (IDX16)
                               <=  LNK-COMB-TEKEDYMD (IDX17))
               CONTINUE
           ELSE
               MOVE    LNK-COMB-HKNCOMBINUM   (IDX17)
                                       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  WRK-HKNCOMBINUM
               MOVE    SDTCHK02-O-HKNCOMBIMEI (IDX17)
                                       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  WRK-HKNCOMBIMEI
      *
               INITIALIZE  WRK-NISSU-DAY-G
               MOVE    WRK-KJNYMD      TO  WRK-YMD
               PERFORM VARYING IDX24   FROM    1   BY  1
                       UNTIL ( IDX24   >   31 )
      *
                   IF    ( THKNCOMBI-DAY  (IDX16 IDX24)
                                       >   ZERO )
      *
                       MOVE    IDX24       TO  WRK-DD
                       IF    ( WRK-YMD
                               >=  LNK-COMB-TEKSTYMD(IDX17) )
                        AND  ( WRK-YMD
                               <=  LNK-COMB-TEKEDYMD(IDX17) )
                           CONTINUE
                       ELSE
                           MOVE    1   TO  WRK-NISSU-DAY (IDX24)
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 200111-NISSU-HEN-SEC
      *
               MOVE    SPACE       TO  WRK-ERRMES-PLUS
               STRING  WRK-ACCTNAME    DELIMITED   BY  SPACE
                       WRK-HKNCOMBINUM DELIMITED   BY  SPACE
                       "（"            DELIMITED   BY  SIZE
                       WRK-HKNCOMBIMEI DELIMITED   BY  SPACE
                       "）"            DELIMITED   BY  SIZE
                       WRK-NISSU-MES   DELIMITED   BY  SPACE
               INTO    WRK-ERRMES-PLUS
               END-STRING
      *
               MOVE    "140002"    TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 215-ERRMES-HEN-SEC
               IF    ( ORCBSD01-CHKKBN (14)    =   "1" )
                   MOVE    1       TO  FLG-CHKEND (14)
               END-IF
           END-IF
      *
           .
       2001111-ACCT-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    地方公費単独組合せチェック
      *****************************************************************
       2001111-CHIHOU-KOH-CHK-SEC      SECTION.
      *
           MOVE    ZERO            TO  CNT-CHIHOU
                                       CNT-UNCHIHOU
                                       FLG-KOHERR
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   4 )
      *
               IF    ( LNK-COMB-KOHHKNNUM (IDX17 IDX0) NOT =   SPACE )
                   IF    ( LNK-COMB-KOHHKNNUM (IDX17 IDX0)
                                          >=   "100" )
                    AND  ( LNK-COMB-KOHHKNNUM (IDX17 IDX0)
                                          <=   "899" )
                       IF    ( LNK-COMB-KOH-HKNKOHSBTKBN (IDX17 IDX0)
                                       NOT =   "6" )
                           COMPUTE CNT-CHIHOU  =   CNT-CHIHOU      +   1
                       END-IF
                   ELSE
                       COMPUTE CNT-UNCHIHOU    =   CNT-UNCHIHOU    +   1
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( CNT-CHIHOU      >   ZERO )
            AND  ( CNT-UNCHIHOU    =   ZERO )
               IF    ( CNT-CHIHOU  =   1 )
                AND  ( LNK-COMB-KOHHKNNUM (IDX17 1)    =   "186" )
                AND  ( SYS-1001-PREFNUM                =   "42" )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-KOHERR
               END-IF
           END-IF
      *
           IF    ( FLG-KOHERR      =   1 )
      *
               MOVE    SDTCHK02-O-HKNCOMBIMEI (IDX17)
                                       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  WRK-HKNCOMBIMEI
               MOVE    THKNCOMBI-DAY-G (IDX16)
                                       TO  WRK-NISSU-DAY-G
               PERFORM 200111-NISSU-HEN-SEC
               MOVE    SPACE           TO  WRK-ERRMES-PLUS
               STRING  WRK-ACCTNAME    DELIMITED   BY  SPACE
                       WRK-HKNCOMBIMEI DELIMITED   BY  SPACE
                       WRK-NISSU-MES   DELIMITED   BY  SPACE
               INTO    WRK-ERRMES-PLUS
               END-STRING
      *
               MOVE    "140003"    TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 215-ERRMES-HEN-SEC
               IF    ( ORCBSD01-CHKKBN (14)    =   "1" )
                   MOVE    1       TO  FLG-CHKEND (14)
               END-IF
           END-IF
      *
           .
       2001111-CHIHOU-KOH-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険・公費の種類チェック処理
      *****************************************************************
       2001111-HKNKOHNUM-CHK-SEC          SECTION.
      *
           MOVE    ZERO        TO      FLG-COMBERR
      *
           IF    ( LNK-COMB-HKNNUM (IDX17) NOT =   SPACE )
               MOVE    LNK-COMB-HKNNUM (IDX17)
                               TO  THKN-SRH-HKNNUM
               MOVE    ZERO    TO  THKN-SRH-PAYKBN
               PERFORM 900-HKNNUM-KEY-SEL-SEC
               IF    ( FLG-HKNNUM  NOT =   ZERO  )
                   MOVE    1       TO      FLG-COMBERR
               END-IF
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   4 )
                    OR   ( FLG-COMBERR     =   1 )
               IF    ( LNK-COMB-KOHHKNNUM (IDX17 IDX0)
                                   NOT =   SPACE )
                   MOVE    LNK-COMB-KOHHKNNUM (IDX17 IDX0)
                               TO  THKN-SRH-HKNNUM
                   MOVE    LNK-COMB-KOHPAYKBN (IDX17 IDX0)
                               TO  THKN-SRH-PAYKBN
                   PERFORM 900-HKNNUM-KEY-SEL-SEC
                   IF    ( FLG-HKNNUM  NOT =   ZERO  )
                       MOVE    1   TO  FLG-COMBERR
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX41   FROM    1   BY  1
                   UNTIL ( IDX41   >   THKNID-MAX )
                    OR   ( FLG-COMBERR     =   1 )
      *
               IF    ( LNK-COMB-HKNID (IDX17)
                                           =   THKNID-HKNID (IDX41))
                AND  ( LNK-COMB-HKNNUM(IDX17)
                                       NOT =   THKNID-HKNNUM(IDX41))
                           MOVE    1       TO  FLG-COMBERR
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   4 )
                    OR   ( FLG-COMBERR     =   1 )
      *
               IF    ( LNK-COMB-KOHHKNNUM (IDX17 IDX0) NOT =   SPACE )
                   PERFORM VARYING IDX41   FROM    1   BY  1
                           UNTIL ( IDX41   >   TKOHID-MAX )
                            OR   ( FLG-COMBERR     =   1 )
      *
                       IF    ( LNK-COMB-KOHID (IDX17 IDX0)
                                           =   TKOHID-KOHID (IDX41))
                        AND  ( LNK-COMB-KOHHKNNUM (IDX17 IDX0)
                                       NOT =   TKOHID-KOHNUM(IDX41))
                           MOVE    1       TO  FLG-COMBERR
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           IF    ( FLG-COMBERR     =   1 )
               MOVE    SDTCHK02-O-HKNCOMBIMEI (IDX17)
                                       TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  WRK-HKNCOMBIMEI
               MOVE    THKNCOMBI-DAY-G (IDX16)
                                       TO  WRK-NISSU-DAY-G
               PERFORM 200111-NISSU-HEN-SEC
               MOVE    SPACE           TO  WRK-ERRMES-PLUS
               STRING  WRK-ACCTNAME    DELIMITED   BY  SPACE
                       WRK-HKNCOMBIMEI DELIMITED   BY  SPACE
                       WRK-NISSU-MES   DELIMITED   BY  SPACE
               INTO    WRK-ERRMES-PLUS
               END-STRING
      *
               MOVE    "140004"    TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 215-ERRMES-HEN-SEC
               IF    ( ORCBSD01-CHKKBN (14)    =   "1" )
                   MOVE    1       TO  FLG-CHKEND (14)
               END-IF
           END-IF
      *
           .
       2001111-HKNKOHNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤の月上限回数、総投与量チェック処理
      *****************************************************************
       200111-YKZTUKIJGN-CHK-SEC       SECTION.
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   TSRYCD-MAX )
      *
               IF    ( TSRYCD-TAISYO (IDX6)    =   1 )
      *
                   MOVE    SPACE       TO  WRK-ERRCD
      *
                   IF    ( TSRYCD-SRYCD (IDX6)(1:1)    =   "6" )
                       MOVE    TSRYCD-SRYCD (IDX6)
                                               TO  WRK-SRYCD
                       PERFORM 900-TENSUPLUS2-KEY-SEL-SEC
                       IF    ( TNSP2-TUKIJGNKAISU  >   ZERO )
                           IF    ( TSRYCD-YKZKAISU (IDX6)
                                       >   TNSP2-TUKIJGNKAISU )
                               MOVE    "190001"
                                           TO  WRK-ERRCD
                           END-IF
                       END-IF
                       IF    ( TNSP2-TUKIJGNTOUYORYO   >   ZERO )
                           IF    ( TSRYCD-SURYOTOTAL (IDX6)
                                       >   TNSP2-TUKIJGNTOUYORYO )
                               MOVE    "190001"
                                           TO  WRK-ERRCD
                           END-IF
                       END-IF
                       IF    ( WRK-ERRCD   NOT =   SPACE )
                           PERFORM 200111-YKZTUKIJGN-ERR-SEC
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       200111-YKZTUKIJGN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤の月上限回数、総投与量エラー処理
      *****************************************************************
       200111-YKZTUKIJGN-ERR-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-ERRMES-PLUS
      *
           MOVE    1               TO  WRK-PNT
      *
           MOVE    TSRYCD-SRYCD (IDX6)
                                   TO  WRK-SRYCD
           MOVE    WRK-KJNYMD      TO  WRK-TNS-YMD
           PERFORM 900-TENSU-KEY-SEL-SEC
           STRING  TNS-NAME        DELIMITED   BY  SPACE
           INTO    WRK-ERRMES-PLUS
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    TSRYCD-YKZKAISU (IDX6)
                                   TO  WRK-STR-LEFT-ZX99
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           MOVE    WRK-STR-LEFT    TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           STRING  "　回数（投薬、注射）："
                                       DELIMITED   BY  SIZE
                   WRK-OUT-INPUT       DELIMITED   BY  SPACE
           INTO    WRK-ERRMES-PLUS
           WITH    POINTER WRK-PNT
           END-STRING
      *
           MOVE    TSRYCD-SURYOTOTAL (IDX6)
                                   TO  WRK-SURYOZ
           PERFORM VARYING IDX0    FROM    5   BY  -1
                   UNTIL ( IDX0     <   1 )
                    OR   ( WRK-SURYOZ2 (IDX0 : 1)
                               NOT ="0" )
               MOVE    SPACE       TO  WRK-SURYOZ2 (IDX0 : 1)
           END-PERFORM
      *
           MOVE    TSRYCD-SURYOTOTAL (IDX6)
                                   TO  WRK-STR-LEFT-ZX99
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           MOVE    WRK-STR-LEFT    TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           STRING  "　投与量："        DELIMITED   BY  SIZE
                   WRK-OUT-INPUT       DELIMITED   BY  SPACE
           INTO    WRK-ERRMES-PLUS
           WITH    POINTER WRK-PNT
           END-STRING
           IF    ( WRK-SURYOZ2     NOT =    SPACE )
               MOVE    WRK-SURYOZ2 TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               STRING  "．"            DELIMITED   BY  SIZE
                       WRK-OUT-INPUT   DELIMITED   BY  SPACE
               INTO    WRK-ERRMES-PLUS
               WITH    POINTER WRK-PNT
               END-STRING
           END-IF
      *
           MOVE    TSRYCD-DAY-G (IDX6) TO  WRK-NISSU-DAY-G
           PERFORM 200111-NISSU-HEN-SEC
           STRING  "　投与日："    DELIMITED   BY  SPACE
                   WRK-NISSU-MES   DELIMITED   BY  SPACE
           INTO    WRK-ERRMES-PLUS
           WITH    POINTER WRK-PNT
           END-STRING
      *
           INITIALIZE              TERRINF-AREA
           PERFORM 215-ERRMES-HEN-SEC
      *
           .
       200111-YKZTUKIJGN-ERR-EXT.
           EXIT.
      *****************************************************************
      *    その他必須チェック処理
      *****************************************************************
       200111-SONOTA-HISSU-CHK-SEC     SECTION.
      *
           IF    ( ORCBSD01-NYUGAIKBN    = "1" )
      *
      *        減算情報チェック処理
               PERFORM 200111-GENZAN-CHK-SEC
      *
      *        入院料加算チェック処理
               PERFORM 200111-NYUKSN-CHK-SEC
      *
           END-IF
      *
           IF    ( FLG-RECEDEN-CHK     =   1 )
               PERFORM 200111-KIZAI-TANICD-CHK-SEC
           END-IF
      *
           IF    ( ORCBSD01-SRYYM     >=  CONST-H2904 )
               PERFORM 200111-KIZAI-MICD-CHK-SEC
           END-IF
      *
           PERFORM 200111-RSIRECEDEN-CHK-SEC
      *
           PERFORM 200111-PTCOM-CHK-SEC
      *
      *    院内・院外混在チェック処理
           PERFORM 200111-INNAI-INGAI-CHK-SEC
      *
      *    内分泌負荷試験チェック処理
           PERFORM 200111-NBFUKA-CHK-SEC
      *
           PERFORM 200111-KAIGO-CHK-SEC
      *
      *
           .
       200111-SONOTA-HISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    減算情報チェック処理
      *****************************************************************
       200111-GENZAN-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX21   FROM    1   BY  1
                   UNTIL ( IDX21   >   CONST-GENZAN-MAX )
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   31 )
      *
                   IF    ( TGENZAN-SRY-DAY  (IDX21 IDX0)    =   1 )
                    AND  ( TGENZAN-NSRY-DAY (IDX21 IDX0)    =   1 )
      *
                       COMPUTE TGENZAN-ERR-CNT (IDX21)
                           =   TGENZAN-ERR-CNT (IDX21)     +   1
      *
                       MOVE    1       TO  TGENZAN-ERR-DAY (IDX21 IDX0)
      *
                   END-IF
      *
               END-PERFORM
      *
               IF    ( TGENZAN-ERR-CNT (IDX21) >   ZERO )
      *
                   MOVE    TGENZAN-SRYCD (IDX21)
                                               TO  WRK-SRYCD
                   MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       NOT =   ZERO )
                       MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
                       PERFORM 800-ZENKAKU-HEN-SEC
                       MOVE    WRK-OUT-INPUT   TO  TNS-NAME
                   END-IF
      *
                   INITIALIZE  WRK-NISSU-AREA
                   MOVE    TGENZAN-ERR-DAY-G (IDX21)
                                               TO  WRK-NISSU-DAY-G
                   PERFORM 200111-NISSU-HEN-SEC
      *
                   MOVE    SPACE       TO  WRK-ERRMES-PLUS
      *
                   STRING  TNS-NAME        DELIMITED   BY  SPACE
                           WRK-NISSU-MES   DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
      *
                   MOVE    "990001"    TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 215-ERRMES-HEN-SEC
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-GENZAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算チェック処理
      *****************************************************************
       200111-NYUKSN-CHK-SEC           SECTION.
      *
           MOVE    ORCBSD01-SRYYM  TO  TNACCT-KKN-STYM
                                       TNACCT-KKN-EDYM
           IF    ( TNACCT-KKN-STDD =   ZERO )
               MOVE    1           TO  TNACCT-KKN-STDD
           END-IF
           IF    ( TNACCT-KKN-EDDD =   ZERO )
               MOVE    WRK-GETUMATU
                                   TO  TNACCT-KKN-EDYMD
           END-IF
      *
           MOVE    SPACE           TO  LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
                                       LNK-SYUDAY-REC-T
      *
           INITIALIZE                  LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
                                       LNK-SYUDAY-REC-T
      *
           MOVE    SPA-HOSPNUM     TO  LNK-S02-HOSPNUM
           MOVE    "1"             TO  LNK-S02-NYUGAIKBN
           MOVE    DTCHK003-PTID   TO  LNK-S02-PTID
           MOVE    TNACCT-KKN-STYMD TO  LNK-S02-SKYSTYMD
           MOVE    TNACCT-KKN-EDYMD TO  LNK-S02-SKYEDYMD
           MOVE    "X"             TO  LNK-S02-TEIKI-KBN
           MOVE    WRK-KJNYMD      TO  LNK-S02-SYSYMD
           MOVE    "9"             TO  LNK-S02-SYORI-KBN
           CALL    "ORCSNYUFTN"    USING
                                       SPA-AREA
                                       LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
                                       LNK-SYUDAY-REC-T
           IF    ( LNK-S02-RC      NOT =   ZERO )
            AND  ( LNK-S02-RC      NOT =   37 )
      *
      *        RC=37（保険組合せなし）はデータチェック本体で編集するので、
      *        無視する。
      *
               PERFORM 900-DBCOMMIT-SEC
               PERFORM 900-DBSTART-SEC
      *
               INITIALIZE              S02MSG-AREA
               MOVE    LNK-S02-RC  TO  S02MSG-S02-RC
               CALL    "ORCS02MSG"     USING
                                       S02MSG-AREA
                                       SPA-AREA
               IF    ( S02MSG-MSG (1)  =   SPACE )
                   MOVE    "990500"    TO  WRK-ERRCD
               ELSE
                   COMPUTE WRK-ERRCD-NUM = 990500 + LNK-S02-RC
                   MOVE    WRK-ERRCD   TO  WRK-ERRCD
               END-IF
      *
               INITIALIZE              TERRINF-AREA
               PERFORM 210-ERRMES-HEN-SEC
           END-IF
      *
           .
       200111-NYUKSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    器材単位コードチェック
      *****************************************************************
       200111-KIZAI-TANICD-CHK-SEC     SECTION.
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   TKZI-MAX )
      *
               INITIALIZE  WRK-NISSU-DAY-G
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( TKZI-DAY (IDX6 IDX-DAY)  >   ZERO )
                       MOVE    1   TO  WRK-NISSU-DAY (IDX-DAY)
                   END-IF
               END-PERFORM
      *
               IF    ( WRK-NISSU-DAY-G    NOT  =   ZERO )
      *
                   PERFORM 200111-NISSU-HEN-SEC
      *
                   MOVE    SPACE          TO  WRK-STR
                   MOVE    SPACE          TO  WRK-ERRMES-PLUS
                   IF    ( TKZI-058-SRYCD (IDX6)   NOT =   SPACE )
                       MOVE    TKZI-058-SRYCD (IDX6)
                                               TO  WRK-SRYCD
                       MOVE    WRK-KJNYMD      TO  WRK-TNS-YMD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       MOVE    TNS-FORMALNAME  TO  WRK-STR
      *
                       MOVE    TKZI-SRYCD (IDX6)   TO  WRK-SRYCD
                       MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       STRING  "商品名称："    DELIMITED   BY  SIZE
                               WRK-STR         DELIMITED   BY  SPACE
                               "　器材マスタ名称："
                                               DELIMITED   BY  SIZE
                               TNS-NAME        DELIMITED   BY  SPACE
                               WRK-NISSU-MES   DELIMITED   BY  SPACE
                       INTO  WRK-ERRMES-PLUS
                       END-STRING
                   ELSE
                       MOVE    TKZI-SRYCD (IDX6)   TO  WRK-SRYCD
                       MOVE    WRK-KJNYMD          TO  WRK-TNS-YMD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       STRING  TNS-NAME        DELIMITED   BY  SPACE
                               WRK-NISSU-MES   DELIMITED   BY  SPACE
                       INTO  WRK-ERRMES-PLUS
                       END-STRING
                   END-IF
      *
                   MOVE    "990006"    TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 215-ERRMES-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       200111-KIZAI-TANICD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    器材単位コードチェック
      *****************************************************************
       200111-KIZAI-MICD-CHK-SEC       SECTION.
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   TKZI-MICD-MAX )
      *
               INITIALIZE  WRK-NISSU-DAY-G
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( TKZI-MICD-DAY (IDX6 IDX-DAY)  >   ZERO )
                       MOVE    1   TO  WRK-NISSU-DAY (IDX-DAY)
                   END-IF
               END-PERFORM
      *
               IF    ( WRK-NISSU-DAY-G    NOT  =   ZERO )
      *
                   PERFORM 200111-NISSU-HEN-SEC
      *
                   MOVE    SPACE          TO  WRK-STR
                   MOVE    SPACE          TO  WRK-ERRMES-PLUS
                   MOVE    TKZI-MICD-SRYCD (IDX6)
                                          TO  WRK-SRYCD
                   MOVE    WRK-KJNYMD     TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
      *
                   MOVE    TKZI-MICD-SRYCD (IDX6)   TO  WRK-SRYCD
                   MOVE    WRK-KJNYMD     TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   STRING  TNS-NAME        DELIMITED   BY  SPACE
                           WRK-NISSU-MES   DELIMITED   BY  SPACE
                   INTO  WRK-ERRMES-PLUS
                   END-STRING
      *
                   MOVE    "990037"    TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 215-ERRMES-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       200111-KIZAI-MICD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    労災レセ電チェック処理
      *****************************************************************
       200111-RSIRECEDEN-CHK-SEC   SECTION.
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   TRSIRECEDEN-MAX )
      *
               INITIALIZE  WRK-NISSU-DAY-G
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( TRSIRECEDEN-DAY (IDX6 IDX-DAY)  >   ZERO )
                       MOVE    1   TO  WRK-NISSU-DAY (IDX-DAY)
                   END-IF
               END-PERFORM
      *
               IF    ( WRK-NISSU-DAY-G    NOT  =   ZERO )
      *
                   PERFORM 200111-NISSU-HEN-SEC
      *
                   MOVE    SPACE          TO  WRK-STR
                   MOVE    SPACE          TO  WRK-ERRMES-PLUS
                   MOVE    TRSIRECEDEN-SRYCD (IDX6)
                                          TO  WRK-SRYCD
                   MOVE    WRK-KJNYMD     TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
      *
                   STRING  TRSIRECEDEN-SRYCD(IDX6) DELIMITED   BY  SIZE
                           "　"                    DELIMITED   BY  SIZE
                           TNS-NAME                DELIMITED   BY  SPACE
                           WRK-NISSU-MES           DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
      *
                   MOVE    WRK-ERRMES-PLUS    TO   WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   MOVE    WRK-OUT-INPUT      TO   WRK-ERRMES-PLUS
      *
                   MOVE    "990030"           TO  WRK-ERRCD
                   IF    ( TRSIRECEDEN-SRYCD (IDX6)(1:5)   =   "09594" )
                       MOVE    "990031"       TO  WRK-ERRCD
                   END-IF
                   INITIALIZE                 TERRINF-AREA
                   PERFORM 215-ERRMES-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       200111-RSIRECEDEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護との給付調整項目突合チェック処理
      *****************************************************************
       200111-KAIGO-CHK-SEC            SECTION.
      *
           PERFORM 900-PTCARE-NINTEI-KEY1-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTCRNIN NOT =   ZERO )
      *
               MOVE    ORCBSD01-SRYYM  TO  WRK-TEKYM
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
      *
                   MOVE    IDXD        TO  WRK-TEKDD
                   IF    ( PTCRNIN-CAREJOTAICD NOT =   "01" )
                       IF    ( PTCRNIN-TEKSTYMD    <=  WRK-TEKYMD )
                        AND  ( PTCRNIN-TEKEDYMD    >=  WRK-TEKYMD )
                           MOVE    1   TO  TKAIGO-NINTEI-DAY (IDXD)
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-PTCARE-NINTEI-KEY1-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   TKAIGO-MAX )
      *
               MOVE    ZERO        TO  TKAIGO-ERR-DAY-G
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
      *
                   IF    ( TKAIGO-NINTEI-DAY (IDXD)        =   1 )
                    AND  ( TKAIGO-SRYCD-DAY  (IDXK IDXD)   =   1 )
                       MOVE    1   TO  TKAIGO-ERR-DAY (IDXD)
                   END-IF
      *
               END-PERFORM
      *
               IF    ( TKAIGO-ERR-DAY-G    NOT =   ZERO )
      *
                   MOVE    TKAIGO-SRYCD (IDXK)
                                           TO  WRK-SRYCD
                   MOVE    WRK-KJNYMD      TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
      *
                   INITIALIZE  WRK-NISSU-DAY-G
                   MOVE    TKAIGO-ERR-DAY-G
                                   TO  WRK-NISSU-DAY-G
                   PERFORM 200111-NISSU-HEN-SEC
      *
                   MOVE    SPACE   TO      WRK-ERRMES-PLUS
                   STRING  TNS-NAME        DELIMITED   BY  SPACE
                           WRK-NISSU-MES   DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
      *
                   MOVE    "990032"    TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 215-ERRMES-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       200111-KAIGO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントチェック処理
      *****************************************************************
       200111-PTCOM-CHK-SEC            SECTION.
      *
           IF    ( TCOMERR-FREE-DAY-G  NOT =   ZERO )
      *
               INITIALIZE  WRK-NISSU-DAY-G
               MOVE    TCOMERR-FREE-DAY-G  TO  WRK-NISSU-DAY-G
               PERFORM 200111-NISSU-HEN-SEC
      *
               MOVE    SPACE       TO  WRK-ERRMES-PLUS
               STRING  "フリーコメント"    DELIMITED   BY  SIZE
                       WRK-NISSU-MES       DELIMITED   BY  SPACE
               INTO    WRK-ERRMES-PLUS
               END-STRING
      *
               MOVE    "990010"    TO  WRK-ERRCD
      *
               INITIALIZE              TERRINF-AREA
               PERFORM 215-ERRMES-HEN-SEC
           END-IF
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   TCOMERR-ZAI-MAX )
      *
               INITIALIZE  WRK-NISSU-DAY-G
               MOVE    TCOMERR-DAY-G (IDX6)
                                   TO  WRK-NISSU-DAY-G
               PERFORM 200111-NISSU-HEN-SEC
      *
               MOVE    SPACE       TO  WRK-ERRMES-PLUS
               STRING  TCOMERR-COM (IDX6)  DELIMITED   BY  SPACE
                       WRK-NISSU-MES       DELIMITED   BY  SPACE
               INTO    WRK-ERRMES-PLUS
               END-STRING
      *
               MOVE    "990010"    TO  WRK-ERRCD
      *
               INITIALIZE              TERRINF-AREA
               PERFORM 215-ERRMES-HEN-SEC
      *
           END-PERFORM
      *
           .
       200111-PTCOM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    院内・院外混在チェック
      *****************************************************************
       200111-INNAI-INGAI-CHK-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-INNAI-DAY-G
                                       FLG-INGAI-DAY-G
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   TSRYCD-MAX )
      *
               IF    ( TSRYCD-SRYCD (IDX6)    =    "113003510"
                                               OR  "113003610"
                                               OR  "113019710"
                                               OR  "113019810"
                                               OR  "113003710"
                                               OR  "113003810"
                                               OR  "113019910"
                                               OR  "113020010"
                                                                )
      *
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
                       IF    ( TSRYCD-DAY (IDX6 IDX-DAY)   >   ZERO )
                        AND  ( TSRYCD-DAY-HKNFLG (IDX6 IDX-DAY)
                                           =   1 )
                        AND  ( TSRYCD-DAY-NYUGAIKBN  (IDX6 IDX-DAY)
                                           =   ORCBSD01-NYUGAIKBN )
      *
                           EVALUATE    TSRYCD-SRYCD (IDX6)
                           WHEN    "113003510"
                           WHEN    "113003610"
                               MOVE    1   TO  FLG-INGAI-DAY (1 IDX-DAY)
                           WHEN    "113019710"
                           WHEN    "113019810"
                               MOVE    1   TO  FLG-INGAI-DAY (2 IDX-DAY)
                           WHEN    "113003710"
                           WHEN    "113003810"
                               MOVE    1   TO  FLG-INNAI-DAY (1 IDX-DAY)
                           WHEN    "113019910"
                           WHEN    "113020010"
                               MOVE    1   TO  FLG-INNAI-DAY (2 IDX-DAY)
                           END-EVALUATE
      *
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDXS    FROM    1   BY  1
                   UNTIL ( IDXS    >   2 )
      *
               PERFORM 2001112-INNAI-INGAI-CHK-SEC
      *
           END-PERFORM
      *
           .
       200111-INNAI-INGAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    院内・院外混在チェック
      *****************************************************************
       2001112-INNAI-INGAI-CHK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-STR
      *
           IF    ( IDXS                =   2 )
               MOVE    "小児かかりつけ診療料"
                                       TO  WRK-STR
           ELSE
               MOVE    "小児科外来診療料"
                                       TO  WRK-STR
           END-IF
      *
           IF    ( FLG-INNAI-DAY-OCC (IDXS)
                                       NOT =   ZERO )
            AND  ( FLG-INGAI-DAY-OCC (IDXS)
                                       NOT =   ZERO )
               INITIALIZE                  WRK-NISSU-AREA
               MOVE    SPACE       TO      WRK-INNAI-MES
                                           WRK-INGAI-MES
                                           WRK-ERRMES-PLUS
      *
               MOVE    FLG-INNAI-DAY-OCC (IDXS)
                                           TO  WRK-NISSU-DAY-G
               PERFORM 200111-NISSU-HEN-SEC
               MOVE    WRK-NISSU-MES       TO  WRK-INNAI-MES
      *
               INITIALIZE                  WRK-NISSU-AREA
               MOVE    FLG-INGAI-DAY-OCC (IDXS)
                                           TO  WRK-NISSU-DAY-G
               PERFORM 200111-NISSU-HEN-SEC
               MOVE    WRK-NISSU-MES       TO  WRK-INGAI-MES
      *
               STRING  WRK-STR             DELIMITED   BY  SPACE
                       "　院内："          DELIMITED   BY  SIZE
                       WRK-INNAI-MES       DELIMITED   BY  SPACE
                       "　院外："          DELIMITED   BY  SIZE
                       WRK-INGAI-MES       DELIMITED   BY  SPACE
               INTO    WRK-ERRMES-PLUS
               END-STRING
               MOVE    "990005"    TO  WRK-ERRCD
               INITIALIZE              TERRINF-AREA
               PERFORM 215-ERRMES-HEN-SEC
           END-IF
      *
           IF    ( ERRKBN-990009       =   "1" )
      *
               IF    ( FLG-INGAI-DAY-OCC (IDXS)
                                       NOT =   ZERO )
                AND  ( FLG-INGAISYOHOU     =   ZERO )
      *
                   INITIALIZE                  WRK-NISSU-AREA
                   MOVE    SPACE       TO      WRK-INGAI-MES
                                               WRK-ERRMES-PLUS
      *
                   INITIALIZE                  WRK-NISSU-AREA
                   MOVE    FLG-INGAI-DAY-OCC (IDXS)
                                               TO  WRK-NISSU-DAY-G
                   PERFORM 200111-NISSU-HEN-SEC
                   MOVE    WRK-NISSU-MES       TO  WRK-INGAI-MES
      *
                   STRING  WRK-STR             DELIMITED   BY  SPACE
                           "（処方せんを交付）"
                                               DELIMITED   BY  SIZE
                           WRK-INGAI-MES       DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
                   MOVE    "990009"    TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 215-ERRMES-HEN-SEC
               END-IF
      *
           END-IF
      *
           .
       2001112-INNAI-INGAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    内分泌負荷試験チェック処理
      *****************************************************************
       200111-NBFUKA-CHK-SEC           SECTION.
      *
           OPEN    INPUT DCSANTEI-FILE
      *
           MOVE    ZERO            TO  FLG-NBFUKACD
                                       FLG-NBFUKATG
      *
           MOVE    ZERO            TO  WRK-SNT-NBFUKATEN
      *
           INITIALIZE                  WRK-NISSU-AREA
      *
           PERFORM 900-DCSANTEI-START-SEC
      *
           PERFORM UNTIL ( FLG-DCSANTEI  NOT =   ZERO )
      *
               IF    ( DCSANTEI-SRYCD        =   CONST-NBHOUKTCD )
                AND  ( DCSANTEI-MSANTEIFLG   NOT =  0 )
                AND  ( DCSANTEI-MSANTEICNT   >   0 )
                   MOVE    1       TO  FLG-NBFUKACD
                   MOVE    DCSANTEI-MSANTEITEN
                                   TO  WRK-SNT-NBFUKATEN
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
      *
                       IF    ( DCSANTEI-DAY (IDX-DAY) >   ZERO )
                           MOVE    1   TO  WRK-NISSU-DAY (IDX-DAY)
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
               IF    ( DCSANTEI-MSANTEITENKBN    =   "2" )
                   MOVE    1       TO  FLG-NBFUKATG
               END-IF
      *
               PERFORM 900-DCSANTEI-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   DCSANTEI-FILE
      *
           IF    ( FLG-NBFUKACD       =    1 )
      *
               IF    ( FLG-NBFUKATG            =   ZERO )
                AND  ( WRK-SNT-NBFUKATEN       >   WRK-TNS-NBFUKATEN )
      *
                   PERFORM 200111-NISSU-HEN-SEC
      *
                   MOVE    SPACE       TO  WRK-ERRMES-PLUS
      *
                   STRING  WRK-NISSU-MES   DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
      *
                   MOVE    "990003"    TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 215-ERRMES-HEN-SEC
      *
               END-IF
      *
               IF    ( FLG-NBFUKATG            =   1 )
                AND  ( WRK-SNT-NBFUKATEN   NOT =   WRK-TNS-NBFUKATEN )
      *
                   PERFORM 200111-NISSU-HEN-SEC
      *
                   MOVE    SPACE       TO  WRK-ERRMES-PLUS
      *
                   STRING  WRK-NISSU-MES   DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
      *
                   MOVE    "990004"    TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 215-ERRMES-HEN-SEC
      *
               END-IF
           END-IF
      *
           .
       200111-NBFUKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日数編集処理
      *****************************************************************
       200111-NISSU-HEN-SEC            SECTION.
      *
           INITIALIZE                      WRK-NISSU-OT
      *
           MOVE    1                   TO  WRK-NISSU-PTR
           MOVE    1                   TO  WRK-NISSU-ST
           MOVE    SPACE               TO  WRK-NISSU-MES
           MOVE    ZERO                TO  WRK-NISSU-KUGIRI-FLG
           MOVE    SPACE               TO  WRK-NISSU-LASTYMD
      *
           PERFORM UNTIL ( WRK-NISSU-ST    >   31 )
      *
               IF    ( WRK-NISSU-DAY (WRK-NISSU-ST )
                                       NOT =   ZERO )
      *
                   MOVE    WRK-NISSU-ST
                                       TO  WRK-DAY
                   MOVE    ORCBSD01-SRYYM
                                       TO  WRK-NISSU-LASTYMD(1:6)
                   MOVE    WRK-DAY-G   TO  WRK-NISSU-LASTYMD(7:2)
      *
                   MOVE    ZERO        TO  WRK-NISSU-RENZOKU-CNT
                   COMPUTE WRK-NISSU-ED    =   WRK-NISSU-ST    +   1
                   PERFORM UNTIL ( WRK-NISSU-ED    >   31 )
                            OR   ( WRK-NISSU-DAY (WRK-NISSU-ED)
                                           =   ZERO )
                       COMPUTE WRK-NISSU-ED
                            =  WRK-NISSU-ED  +   1
      *
                       COMPUTE WRK-NISSU-RENZOKU-CNT
                            =  WRK-NISSU-RENZOKU-CNT   +   1
      *
                   END-PERFORM
      *
                   IF    ( WRK-NISSU-KUGIRI-FLG   =   ZERO )
                       STRING  "　"            DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
      *
                       MOVE    1       TO  WRK-NISSU-KUGIRI-FLG
      *
                   ELSE
                       STRING  "，"            DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
                   END-IF
      *
                   IF    ( WRK-NISSU-RENZOKU-CNT  >   ZERO )
                       COMPUTE WRK-NISSU-ED    =   WRK-NISSU-ED    -   1
      *
                       IF    ( WRK-NISSU-ST    <   10 )
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-9
                       ELSE
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-Z9
                       END-IF
      *
                       IF    ( WRK-NISSU-ED    <   10 )
                           MOVE    WRK-NISSU-ED    TO  WRK-NISSU-ED-9
                       ELSE
                           MOVE    WRK-NISSU-ED    TO  WRK-NISSU-ED-Z9
                       END-IF
      *
                       STRING  WRK-NISSU-ST-X      DELIMITED   BY  SPACE
                               "日"                DELIMITED   BY  SIZE
                               "〜"                DELIMITED   BY  SIZE
                               WRK-NISSU-ED-X      DELIMITED   BY  SPACE
                               "日"                DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
                       COMPUTE WRK-NISSU-ST
                           =   WRK-NISSU-ED      +   1
                   ELSE
      *
                       IF    ( WRK-NISSU-ST    <   10 )
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-9
                       ELSE
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-Z9
                       END-IF
      *
                       STRING  WRK-NISSU-ST-X      DELIMITED   BY  SPACE
                               "日"                DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
                       COMPUTE WRK-NISSU-ST
                           =   WRK-NISSU-ST      +   1
                   END-IF
      *
               ELSE
                   COMPUTE WRK-NISSU-ST
                       =   WRK-NISSU-ST      +   1
               END-IF
           END-PERFORM
      *
           MOVE    WRK-NISSU-MES       TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT       TO  WRK-NISSU-MES
      *
           .
       200111-NISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    データチェック処理（チェックマスタ以外のマスタ）
      *****************************************************************
       20011-DATA-CHK-MST-OTHERS-SEC   SECTION.
      *
      *    算定回数チェック処理
           PERFORM 200111-ETENSU5-CHK-SEC
      *
           IF    ( ORCBSD01-CHKKBN (21)    =   "2" )
      *        選択式コメント登録漏れチェック処理
               PERFORM 200111-RKISAI-CHK-SEC
           END-IF
      *
           .
       20011-DATA-CHK-MST-OTHERS-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表チェック処理
      *****************************************************************
       200111-ETENSU5-CHK-SEC          SECTION.
      *
           PERFORM 900-DTCHK013-START-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK013    NOT =   ZERO )
      *
               MOVE    1               TO  FLG-KAISU-CHK
      *
               MOVE    ZERO            TO  FLG-HKNKBN
               PERFORM VARYING IDX42   FROM    1   BY  1
                       UNTIL ( IDX42   >   DTCHK013-HKNCOMBI-MAX )
                        OR   ( FLG-HKNKBN  =   1 )
      *
                   MOVE    DTCHK013-HKNCOMBI (IDX42)
                                       TO  WRK-HKNCOMBI
                   PERFORM 220-HKNCOMBI-GET-SEC
                   MOVE    COMB-HKNNUM
                                  TO   WRK-ZAIHKNNUM
                   IF    ( COMB-KOHHKNNUM (1)  =   "970" )
                       MOVE    COMB-KOHHKNNUM (1)
                                  TO   WRK-ZAIHKNNUM
                   END-IF
                   PERFORM 2001111-TAISYO-HKN-CHK-SEC
                   IF    ( FLG-TAISYO-HKN  =   1 )
                       MOVE    1  TO   FLG-HKNKBN
                   END-IF
      *
               END-PERFORM
      *
               IF    ( FLG-HKNKBN              =   ZERO )
                   MOVE    ZERO        TO  FLG-KAISU-CHK
               END-IF
      *
               IF    ( DTCHK013-JGNCNTERR      =   9 )
                AND  ( SYS-1101-SRYKAISUWRNKBN =   "2" )
                   MOVE    ZERO        TO  FLG-KAISU-CHK
               END-IF
      *
               IF    ( DTCHK013-NYUGAIKBN  NOT =   ORCBSD01-NYUGAIKBN )
                   MOVE    ZERO        TO  FLG-KAISU-CHK
               END-IF
      *
               IF    ( FLG-KAISU-CHK   =   1 )
                   IF    ( DTCHK013-KAISU1    >    ZERO )
                       EVALUATE    TRUE
                       WHEN    ( DTCHK013-TANICD   =   132 )
                        AND    ( DTCHK013-SRYYMD
                                           NOT =   "00000000" )
                        AND    ( DTCHK013-SRYYMD
                                           NOT =   DTCHK013-NYUINYMD )
                           MOVE    "200101"    TO  WRK-ERRCD
                           PERFORM 2001112-ETENSU5-CHK-SEC
                       WHEN    ( DTCHK013-TANICD   =   134 )
                        AND    ( DTCHK013-TAIINYMD
                                           NOT =   "99999999" )
                        AND    ( DTCHK013-SRYYMD
                                           NOT =   "00000000" )
                        AND    ( DTCHK013-SRYYMD
                                           NOT =   DTCHK013-TAIINYMD )
                           MOVE    "200102"    TO  WRK-ERRCD
                           PERFORM 2001112-ETENSU5-CHK-SEC
                       WHEN    OTHER
                           IF  ( DTCHK013-KAISU-TOTAL
                                           >   DTCHK013-LIMIT )
                               PERFORM 2001111-ETENSU5-CHK-SEC
                           END-IF
                       END-EVALUATE
                   END-IF
               END-IF
      *
               MOVE    DTCHK013-REC    TO  WKDTCHK013-REC
      *
               PERFORM 900-DTCHK013-READ-SEC
      *
      *        異なるマスタで同じチェックを行っている場合はスキップする。
               PERFORM UNTIL ( FLG-DTCHK013
                                       NOT =   ZERO )
                        OR   ( DTCHK013-PTID
                                       NOT =   WKDTCHK013-PTID )
                        OR   ( DTCHK013-TANICD
                                       NOT =   WKDTCHK013-TANICD )
                        OR   ( DTCHK013-SRYCD
                                       NOT =   WKDTCHK013-SRYCD )
                        OR   ( DTCHK013-SHONUM
                                       NOT =   WKDTCHK013-SHONUM )
                        OR   ( DTCHK013-SRYYMD
                                       NOT =   WKDTCHK013-SRYYMD )
                        OR   ( DTCHK013-SRYSTYMD
                                       NOT =   WKDTCHK013-SRYSTYMD )
                        OR   ( DTCHK013-SRYEDYMD
                                       NOT =   WKDTCHK013-SRYEDYMD )
      *
                   PERFORM 900-DTCHK013-READ-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           .
       200111-ETENSU5-CHK-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表チェック処理
      *****************************************************************
       2001111-ETENSU5-CHK-SEC          SECTION.
      *
           MOVE    DTCHK013-SRYCD  TO  WRK-SRYCD
           MOVE    WRK-KJNYMD      TO  WRK-TNS-YMD
           PERFORM 900-TENSU-KEY-SEL-SEC
      *
           MOVE    "200001"        TO  WRK-ERRCD
           INITIALIZE                      TERRINF-AREA
           MOVE    DTCHK013-SRYCD      TO  TERRINF-SRYCD
           MOVE    DTCHK013-TANICD     TO  TERRINF-TANICD
           MOVE    DTCHK013-TANINAME   TO  TERRINF-TANINAME
           MOVE    DTCHK013-SHONUM     TO  TERRINF-SHONUM
           MOVE    DTCHK013-SRYSTYMD   TO  TERRINF-SRYSTYMD
           MOVE    DTCHK013-SRYEDYMD   TO  TERRINF-SRYEDYMD
           MOVE    DTCHK013-LIMIT      TO  TERRINF-LIMIT
           MOVE    DTCHK013-TSUKISU    TO  TERRINF-TSUKISU
           MOVE    SPACE       TO  WRK-ERRMES-PLUS
      *
           INITIALIZE              WRK-ERR-ITEM-G
           MOVE    DTCHK013-LIMIT  TO  WRK-STR-LEFT-ZX99
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           MOVE    WRK-STR-LEFT    TO  WRK-ERR-ITEM (1)
           MOVE    DTCHK013-KAISU-TOTAL
                                           TO  WRK-STR-LEFT-ZX99
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           MOVE    WRK-STR-LEFT    TO  WRK-ERR-ITEM (2)
      *
           IF    ( DTCHK013-OUTYMD NOT =   SPACE )
               IF    ( DTCHK013-KAISU-TOTAL
                                       =   DTCHK013-KAISU2 + 1 )
                   MOVE    DTCHK013-KAISU-TOTAL
                                   TO  WRK-STR-LEFT-ZX99
                   PERFORM 800-STR-HIDARIZUME-HEN-SEC
                   STRING  "（今回"      DELIMITED BY SIZE
                           WRK-STR-LEFT    DELIMITED BY SPACE
                           "回目）"        DELIMITED BY SPACE
                   INTO    WRK-ERR-ITEM (3)
                   END-STRING
               ELSE
                   COMPUTE WRK-KAISU   =   DTCHK013-KAISU2 + 1
                   MOVE    WRK-KAISU   TO  WRK-STR-LEFT-ZX99
                   PERFORM 800-STR-HIDARIZUME-HEN-SEC
                   MOVE    WRK-STR-LEFT TO WRK-KAISU-X
                   MOVE    DTCHK013-KAISU-TOTAL
                                       TO  WRK-STR-LEFT-ZX99
                   PERFORM 800-STR-HIDARIZUME-HEN-SEC
                   STRING  "（今回"        DELIMITED BY SIZE
                           WRK-KAISU-X     DELIMITED BY SPACE
                           "〜"            DELIMITED BY SIZE
                           WRK-STR-LEFT    DELIMITED BY SPACE
                           "回目）"        DELIMITED BY SPACE
                   INTO    WRK-ERR-ITEM (3)
                   END-STRING
               END-IF
      *
               MOVE    DTCHK013-KAISU2 TO  WRK-STR-LEFT-ZX99
               PERFORM 800-STR-HIDARIZUME-HEN-SEC
               MOVE    DTCHK013-OUTYMD TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               STRING  "（診療月前の最終算定日："
                                       DELIMITED BY SIZE
                       WRK-EDTYMD3     DELIMITED BY SPACE
                       "）"            DELIMITED BY SIZE
               INTO    WRK-ERR-ITEM (4)
               END-STRING
           END-IF
      *
           INITIALIZE                  WRK-NISSU-AREA
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               IF    ( DTCHK013-DAY (IDX-DAY) >   ZERO )
                   MOVE    1   TO  WRK-NISSU-DAY (IDX-DAY)
               END-IF
           END-PERFORM
           PERFORM 200111-NISSU-HEN-SEC
           MOVE    WRK-NISSU-LASTYMD   TO   TERRINF-LASTYMD
      *
           STRING  TNS-NAME                DELIMITED   BY  SPACE
                   "　上限"                DELIMITED   BY  SIZE
                   "（"                    DELIMITED   BY  SIZE
                   DTCHK013-TANINAME       DELIMITED   BY  SPACE
                   "）："                  DELIMITED   BY  SIZE
                   WRK-ERR-ITEM (1)        DELIMITED   BY  SPACE
                   "　回数："              DELIMITED   BY  SIZE
                   WRK-ERR-ITEM (2)        DELIMITED   BY  SPACE
                   WRK-ERR-ITEM (3)        DELIMITED   BY  SPACE
                   WRK-NISSU-MES           DELIMITED   BY  SPACE
                   WRK-ERR-ITEM (4)        DELIMITED   BY  SPACE
           INTO    WRK-ERRMES-PLUS
           END-STRING
           MOVE    WRK-ERRMES-PLUS TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-ERRMES-PLUS
           PERFORM 215-ERRMES-HEN-SEC
      *
           .
       2001111-ETENSU5-CHK-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表チェック処理
      *****************************************************************
       2001112-ETENSU5-CHK-SEC          SECTION.
      *
           MOVE    DTCHK013-SRYCD TO  WRK-SRYCD
           MOVE    WRK-KJNYMD     TO  WRK-TNS-YMD
           PERFORM 900-TENSU-KEY-SEL-SEC
      *
           INITIALIZE              TERRINF-AREA
           MOVE    DTCHK013-SRYCD      TO  TERRINF-SRYCD
           MOVE    DTCHK013-TANICD     TO  TERRINF-TANICD
           MOVE    DTCHK013-TANINAME   TO  TERRINF-TANINAME
           MOVE    DTCHK013-SHONUM     TO  TERRINF-SHONUM
           MOVE    DTCHK013-SRYSTYMD   TO  TERRINF-SRYSTYMD
           MOVE    DTCHK013-SRYEDYMD   TO  TERRINF-SRYEDYMD
           MOVE    DTCHK013-LIMIT      TO  TERRINF-LIMIT
           MOVE    SPACE           TO  WRK-ERRMES-PLUS
      *
           INITIALIZE              WRK-ERR-ITEM-G
           MOVE    DTCHK013-LIMIT  TO  WRK-STR-LEFT-ZX99
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           MOVE    WRK-STR-LEFT    TO  WRK-ERR-ITEM (1)
      *
           INITIALIZE                  WRK-NISSU-AREA
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               IF    ( DTCHK013-DAY (IDX-DAY) >   ZERO )
                   MOVE    1   TO  WRK-NISSU-DAY (IDX-DAY)
               END-IF
           END-PERFORM
           PERFORM 200111-NISSU-HEN-SEC
           MOVE    WRK-NISSU-LASTYMD   TO   TERRINF-LASTYMD
      *
           STRING  TNS-NAME                DELIMITED   BY  SPACE
                   WRK-NISSU-MES           DELIMITED   BY  SPACE
           INTO    WRK-ERRMES-PLUS
           END-STRING
           MOVE    WRK-ERRMES-PLUS TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-ERRMES-PLUS
           PERFORM 215-ERRMES-HEN-SEC
      *
           .
       2001112-ETENSU5-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択式コメント登録漏れチェック処理
      *****************************************************************
       200111-RKISAI-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   TSRYCD-MAX )
      *
               MOVE    "21"        TO  WRK-REIGAI-CHKNUM
               MOVE    TSRYCD-SRYCD (IDX6)
                                   TO  WRK-REIGAI-CD
               PERFORM 800-REIGAI-CHK-SEC
               IF    ( FLG-REIGAI  =   ZERO )
                   PERFORM 200112-RKISAI-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       200111-RKISAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択式コメント登録漏れチェック処理
      *****************************************************************
       200112-RKISAI-CHK-SEC           SECTION.
      *
           MOVE    1                   TO  FLG-TARGET
      *
           IF    ( TSRYCD-NYUGAIKBN (IDX6) =   ORCBSD01-NYUGAIKBN )
               CONTINUE
           ELSE
               MOVE    ZERO            TO  FLG-TARGET
           END-IF
      *
           IF    ( FLG-TARGET          =   1 )
      *
               INITIALIZE                  WRK-NISSU-DAY-G
               INITIALIZE                  FLG-SRYSNT-G
      *
               MOVE    TSRYCD-SRYCD (IDX6)
                                       TO  KEY-SRYCD
      *
               PERFORM 900-DTCHK022-START-SEC
      *
               PERFORM UNTIL ( FLG-DTCHK022    NOT =   ZERO )
                         OR  ( FLG-SRYSNT-MONTHERR =   1 )
      *
                   MOVE    1               TO  FLG-SRYSNT-MONTHERR
                   MOVE    ZERO            TO  CNT-COMMENT
      *
                   MOVE    DTCHK022-RKISAI-AREA
                                           TO  RKISAI-REC
      *
                   EVALUATE    TRUE
                   WHEN  ( DTCHK022-JYOKEN     =   1 )
                   WHEN  ( DTCHK022-JYOKEN     =   2 )
                    AND  ( DTCHK022-NYUGAIKBN  =   ORCBSD01-NYUGAIKBN )
                   WHEN  ( DTCHK022-JYOKEN     =   3 )
                    AND  ( TSRYCD-KAISU2 (IDX6)
                                              >=   DTCHK022-KAISU )
                       PERFORM 2001121-RKISAI-CHK-SEC
                   WHEN    OTHER
                       MOVE    2           TO  FLG-SRYSNT-MONTHERR
      *
                   END-EVALUATE
      *
      *            FLG-SRYSNT-MONTHERR = 2 でBREAKした場合の考慮
                   PERFORM UNTIL ( FLG-DTCHK022
                                           NOT =  ZERO )
                            OR   ( RKISAI-ITEMNO
                                           NOT =  DTCHK022-ITEMNO )
                            OR   ( RKISAI-EDANO
                                           NOT =  DTCHK022-EDANO )
                       PERFORM 900-DTCHK022-READ-SEC
                   END-PERFORM
      *
               END-PERFORM
      *
               IF    ( FLG-SRYSNT-MONTHERR =   1 )
      *
                   PERFORM VARYING IDX10   FROM    1   BY  1
                           UNTIL ( IDX10   >   31 )
                       IF    ( TSRYCD-DAY-HKNFLG (IDX6 IDX10)
                                           =   1 )
                           IF  ( TSRYCD-DAY-NYUGAIKBN (IDX6 IDX10)
                                           =   ORCBSD01-NYUGAIKBN )
                               MOVE  1     TO  WRK-NISSU-DAY (IDX10)
                           END-IF
                       END-IF
                   END-PERFORM
      *
                   IF    ( WRK-NISSU-DAY-G NOT =   ZERO )
                       MOVE    "210001"    TO  WRK-ERRCD
                       MOVE    TSRYCD-SRYCD (IDX6)
                                           TO  WRK-SRYCD
                       MOVE    WRK-KJNYMD  TO  WRK-TNS-YMD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       IF    ( FLG-TENSU       NOT =   ZERO )
                           MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
                           PERFORM 800-ZENKAKU-HEN-SEC
                           MOVE    WRK-OUT-INPUT   TO  TNS-NAME
                       END-IF
      *
                       PERFORM 200111-NISSU-HEN-SEC
                       MOVE    WRK-NISSU-LASTYMD
                                           TO   WRK-LASTYMD
      *
                       MOVE    SPACE       TO   WRK-ERRMES-PLUS
      *
                       STRING TNS-NAME         DELIMITED   BY  SPACE
                              WRK-NISSU-MES    DELIMITED   BY SPACE
                       INTO   WRK-ERRMES-PLUS
                       END-STRING
      *
                       INITIALIZE              TERRINF-AREA
                       MOVE    TSRYCD-SRYCD (IDX6)
                                           TO  TERRINF-SRYCD
                       MOVE    WRK-LASTYMD TO  TERRINF-LASTYMD
                       PERFORM 215-ERRMES-HEN-SEC
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       200112-RKISAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択式コメント登録漏れチェック処理
      *****************************************************************
       2001121-RKISAI-CHK-SEC           SECTION.
      *
           PERFORM UNTIL ( FLG-DTCHK022    NOT =   ZERO )
                    OR   ( RKISAI-ITEMNO   NOT =   DTCHK022-ITEMNO )
                    OR   ( RKISAI-EDANO    NOT =   DTCHK022-EDANO )
                    OR   ( FLG-SRYSNT-MONTHERR
                                           =   2 )
      *
               PERFORM VARYING IDX32   FROM   1    BY  1
                       UNTIL ( IDX32   >   TSRYCD-MAX )
      *
                   IF    ( DTCHK022-C-CD
                                       =   TSRYCD-SRYCD (IDX32))
                       IF    ( TSRYCD-NYUGAIKBN (IDX32)
                                       =   ORCBSD01-NYUGAIKBN )
                        AND  ( TSRYCD-HKNFLG (IDX32)
                                       =   1 )
      *
                           IF    ( DTCHK022-JYOKEN =   3 )
                               COMPUTE CNT-COMMENT
                                   =   CNT-COMMENT +   1
                           ELSE
                               MOVE    2   TO FLG-SRYSNT-MONTHERR
                           END-IF
      *
                           MOVE    TSRYCD-MAX  TO  IDX32
      *
                       END-IF
                   END-IF
               END-PERFORM
      *
               PERFORM 900-DTCHK022-READ-SEC
      *
           END-PERFORM
      *
           IF    ( RKISAI-JYOKEN             =   3 )
      *        IF    ( TSRYCD-KAISU2 (IDX6)   <=   CNT-COMMENT )
      *            MOVE    2   TO FLG-SRYSNT-MONTHERR
      *        END-IF
               IF    ( CNT-COMMENT           >   ZERO )
                   MOVE    2   TO FLG-SRYSNT-MONTHERR
               END-IF
           END-IF
      *
           .
       2001121-RKISAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    禁忌チェック項目情報退避処理
      *****************************************************************
       20012-CHK-KINKI-TAIHI-SEC       SECTION.
      *
      *    退避エリア初期処理
           PERFORM 200121-TAIHI-AREA-INIT-SEC
      *
      *    診療情報退避処理
           PERFORM 200121-SRYINF-TAIHI-SEC
      *
           .
       20012-CHK-KINKI-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    退避エリア初期処理
      *****************************************************************
       200121-TAIHI-AREA-INIT-SEC      SECTION.
      *
           INITIALIZE                  TSRYCD-AREA
                                       TINGAI-AREA
                                       PTACCT-AREA
                                       PTSRY-AREA
                                       PTZAI-AREA
      *
           PERFORM 200111-TSRYKA-AREA-INIT-SEC
      *
           .
       200121-TAIHI-AREA-INIT-EXT.
           EXIT.
      *****************************************************************
      *    診療情報情報退避処理
      *****************************************************************
       200121-SRYINF-TAIHI-SEC         SECTION.
      *
           PERFORM 200111-DCSRY-TAIHI-SEC
      *
           PERFORM 200121-DCACCT-TAIHI-SEC
      *
           INITIALIZE                  KEY-AREA
      *
           PERFORM VARYING IDX-ZAI FROM    1   BY  1
                   UNTIL ( IDX-ZAI >   PTZAI-MAX )
      *
               PERFORM VARYING IDX-ACCT    FROM    1   BY  1
                       UNTIL ( IDX-ACCT    >   PTACCT-MAX )
                   IF    ( PTZAI-ZAINUM (IDX-ZAI)
                                       =   PTACCT-ZAINUM (IDX-ACCT))
                    AND  ( PTZAI-ACCTKBN(IDX-ZAI)
                                       =   PTACCT-ACCTKBN(IDX-ACCT))
                       MOVE    PTACCT-OCC (IDX-ACCT)
                                       TO  DCACCT-REC
                       IF ( DCACCT-ACCTRENNUM  =   1 )
      *
                           PERFORM 2001211-SRYINF-TAIHI-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
           END-PERFORM
      *
           .
       200121-SRYINF-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    会計情報情報退避処理
      *****************************************************************
       200121-DCACCT-TAIHI-SEC         SECTION.
      *
           PERFORM 910-DCACCT-START-SEC
      *
           PERFORM UNTIL ( FLG-DCACCT NOT =   ZERO )
                    OR   ( PTACCT-MAX    >=   CONST-PTACCT-MAX )
      *
               COMPUTE PTACCT-MAX  =   PTACCT-MAX   +   1
      *
               MOVE    DCACCT-REC  TO  PTACCT-OCC (PTACCT-MAX)
      *
               PERFORM 200111-PTZAI-TAIHI-SEC
      *
               PERFORM 910-DCACCT-READ-SEC
      *
           END-PERFORM
      *
           .
       200121-DCACCT-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    診療情報情報退避処理
      *****************************************************************
       2001211-SRYINF-TAIHI-SEC        SECTION.
      *
           MOVE    ZERO        TO  IDX-TSRYKA
           MOVE    ZERO        TO  FLG-TSRYKA-OVER
           PERFORM 200111-TSRYKA-SRYKA-HEN-SEC
      *
           PERFORM VARYING IDX-SRY FROM    1   BY  1
                   UNTIL ( IDX-SRY     >   PTSRY-MAX )
      *
               IF    ( DCACCT-ZAINUM   =   PTSRY-ZAINUM (IDX-SRY) )
                AND  ( DCACCT-ACCTKBN  =   PTSRY-ACCTKBN(IDX-SRY) )
      *
                   MOVE    PTSRY-OCC (IDX-SRY)
                                       TO  DCSRY-REC
      *
                   IF    ( DCSRY-SRYCD (1:1)   =   "6" )
      *
                       IF    ( FLG-TSRYKA-OVER =   ZERO )
                        AND  ( IDX-TSRYKA      >   ZERO )
      *                    診療科退避ワーク診療行為コード編集処理
                           PERFORM 200111-TSRYKA-SRYCD-HEN-SEC
                       END-IF
      *
      *                診療行為コード退避ワーク編集処理
                       PERFORM 200111-TSRYCD-HEN-SEC
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    DCACCT-HKNNUM   TO  WRK-ZAIHKNNUM
           EVALUATE    TRUE
           WHEN  ( DCACCT-KOHNUM   =   "970" )
               MOVE    DCACCT-KOHNUM
                                   TO  WRK-ZAIHKNNUM
           WHEN  ( DCACCT-HKNCOMBI =   9999 )
               MOVE    "HHH"
                                   TO  WRK-ZAIHKNNUM
           END-EVALUATE
           PERFORM 2001111-TAISYO-HKN-CHK-SEC
           IF    ( FLG-TAISYO-HKN  =   1 )
               MOVE    ZERO        TO  IDXH
               MOVE    DCACCT-HKNNUM       TO  WRK-HKNNUM
               IF    ( WRK-HKNNUM      =   SPACE )
                   MOVE    ZERO        TO  WRK-HKNNUM
               END-IF
               IF    ( DCACCT-KOHNUM   =   "970" )
                   MOVE    "970"       TO  WRK-HKNNUM
               END-IF
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-TINGAI-MAX )
                        OR   ( IDXH    >   ZERO )
                   IF    ( TINGAI-HKNNUM (IDX0)
                                       =   WRK-HKNNUM
                                        OR SPACE )
                       MOVE    IDX0    TO  IDXH
                                           TINGAI-MAX
                   END-IF
               END-PERFORM
               IF    ( IDXH            >   ZERO )
                   IF    ( TINGAI-HKNNUM (IDXH)
                                       =   SPACE )
                       MOVE    WRK-HKNNUM
                                       TO  TINGAI-HKNNUM (IDXH)
                   END-IF
                   IF    ( DCACCT-NYUGAIKBN
                                       =   ORCBSD01-NYUGAIKBN )
                       MOVE    DCACCT-NYUGAIKBN
                                       TO  TINGAI-NYUGAIKBN (IDXH)
                   END-IF
                   COMPUTE TINGAI-TEN (IDXH)
                       =   TINGAI-TEN (IDXH)
                       + ( DCACCT-INGAITEN
                       *   DCACCT-ZAIKAISU )
               END-IF
           END-IF
      *
           .
       2001211-SRYINF-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    禁忌チェック処理
      *****************************************************************
       20012-KINKI-CHK-SEC             SECTION.
      *
      *    データチェック初期処理
           PERFORM 20011-DATA-CHK-INIT-SEC
      *
      *    薬剤の月上限回数・総投与量
           IF    ( ORCBSD01-CHKKBN(19) NOT =   ZERO  )
               PERFORM 200111-YKZTUKIJGN-CHK-SEC
           END-IF
      *
      *    併用禁忌の薬剤チェック処理
           IF    ( ORCBSD01-CHKKBN(10) NOT =   ZERO  )
               PERFORM 200121-HEI-YKZ-CHK-SEC
           END-IF
      *
      *    併用禁忌の薬剤チェック処理
           IF    ( ORCBSD01-CHKKBN(15) NOT =   ZERO  )
               PERFORM 200121-HEI-YKZKNKBYO-CHK-SEC
           END-IF
      *
           IF    ( ORCBSD01-INGAIPROCKBN   NOT =   ZERO )
               PERFORM 200121-INGAITEN-CHK-SEC
           END-IF
      *
           .
       20012-KINKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    併用禁忌の薬剤チェック処理
      *****************************************************************
       200121-HEI-YKZ-CHK-SEC          SECTION.
      *
           IF    ( ORCBSD01-CHKKBN(10) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (10)
           END-IF
      *
      *    併用禁忌の薬剤チェック処理
           IF    ( FLG-CHKEND (10)     =   ZERO )
            AND  ( FLG-YKZHEI          =   1 )
               PERFORM 200112-HEI-YKZ-CHK-SEC
           END-IF
      *
           .
       200121-HEI-YKZ-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤と投与禁忌病名チェック処理
      *****************************************************************
       200121-HEI-YKZKNKBYO-CHK-SEC    SECTION.
      *
           IF    ( ORCBSD01-CHKKBN(15) =   ZERO  )
               MOVE    1               TO  FLG-CHKEND (15)
           END-IF
      *
      *    併用禁忌の薬剤チェック処理
           IF    ( FLG-CHKEND (15)     =   ZERO )
            AND  ( FLG-YKZKNKBYO       =   1 )
               PERFORM 200112-HEI-YKZKNKBYO-CHK-SEC
           END-IF
      *
           .
       200121-HEI-YKZKNKBYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    院外処方点数チェック処理
      *****************************************************************
       200121-INGAITEN-CHK-SEC    SECTION.
      *
           MOVE    ZERO            TO  WRK-INGAITEN
      *
           EVALUATE    DTCHK003-HKNKBN
           WHEN    CONST-SYAHO
               MOVE    SYS-1101-SYA-INGAITEN
                                   TO  WRK-INGAITEN
           WHEN    CONST-KOKUHO
               MOVE    SYS-1101-KOK-INGAITEN
                                   TO  WRK-INGAITEN
           WHEN    CONST-ROSAI
               MOVE    SYS-1101-RSI-INGAITEN
                                   TO  WRK-INGAITEN
           WHEN    CONST-JIBAI
           WHEN    CONST-DAISAN
               MOVE    SYS-1101-JBI-INGAITEN
                                   TO  WRK-INGAITEN
           WHEN    CONST-JIHI
               MOVE    SYS-1101-JIH-INGAITEN
                                   TO  WRK-INGAITEN
           WHEN    CONST-KOUGAI
               MOVE    SYS-1101-KGI-INGAITEN
                                   TO  WRK-INGAITEN
           WHEN    CONST-KOUKI
               MOVE    SYS-1101-KOU-INGAITEN
                                   TO  WRK-INGAITEN
           WHEN    CONST-CHIKEN
               MOVE    SYS-1101-CKN-INGAITEN
                                   TO  WRK-INGAITEN
           END-EVALUATE
           IF    ( WRK-INGAITEN    >   ZERO )
             PERFORM VARYING IDXH    FROM    1   BY  1
                   UNTIL ( IDXH    >   TINGAI-MAX )
               IF   ( TINGAI-NYUGAIKBN (IDXH)
                                          =  ORCBSD01-NYUGAIKBN )
                AND ( TINGAI-TEN (IDXH)  >=  WRK-INGAITEN )
                   MOVE    "990019"    TO  WRK-ERRCD
                   MOVE    TINGAI-HKNNUM (IDXH)
                                       TO  SRH-HKNNUM
                   INITIALIZE              TERRINF-AREA
                   MOVE    SPACE       TO  WRK-ERRMES-PLUS
                   MOVE    TINGAI-TEN (IDXH)
                                       TO  WRK-STR-LEFT-ZX99
                   PERFORM 800-STR-HIDARIZUME-HEN-SEC
                   MOVE    WRK-STR-LEFT    TO  WRK-MAE-INPUT
                   PERFORM 800-ZENKAKU-HEN-SEC
                   STRING  "院外処方合計点数："
                                           DELIMITED   BY  SIZE
                           WRK-OUT-INPUT   DELIMITED   BY  SPACE
                           "点"            DELIMITED   BY  SPACE
                   INTO    WRK-ERRMES-PLUS
                   END-STRING
                   PERFORM 216-ERRMES-HEN-SEC
               END-IF
             END-PERFORM
           END-IF
      *
           .
       200121-INGAITEN-CHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（メッセージに付加する情報なし）
      *****************************************************************
       210-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 220-REIGAI-CHK-SEC
           IF    ( FLG-REIGAI    =     ZERO )
               PERFORM 2101-ERRMES-HEN-SEC
           END-IF
      *
           .
       210-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（メッセージに付加する情報なし）
      *****************************************************************
       2101-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
      *    チェック結果ファイル共通編集項目編集処理
           PERFORM 2111-DTCHK004-KYOTU-HEN-SEC
      *
           MOVE    WRK-ERRMES      TO  DTCHK004-ERRMSG
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
      *    DISPLAY "DTCHK004-REC = " DTCHK004-REC
      *
           .
       2101-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（１）
      *****************************************************************
       211-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 220-REIGAI-CHK-SEC
           IF    ( FLG-REIGAI    =     ZERO )
               PERFORM 2111-ERRMES-HEN-SEC
           END-IF
      *
           .
       211-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（１）
      *****************************************************************
       2111-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 2111-HKNNUM-GET-SEC
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
      *    チェック結果ファイル共通編集項目編集処理
           PERFORM 2111-DTCHK004-KYOTU-HEN-SEC
      *
           IF    ( SRH-HKNNUMMEI       =   SPACE )
               MOVE    WRK-ERRMES      TO  DTCHK004-ERRMSG
           ELSE
               STRING  "［"            DELIMITED   BY  SIZE
                       SRH-HKNNUMMEI   DELIMITED   BY  SPACE
                       "］"            DELIMITED   BY  SIZE
                       WRK-ERRMES      DELIMITED   BY  SPACE
               INTO    DTCHK004-ERRMSG
               END-STRING
           END-IF
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
      *    DISPLAY "DTCHK004-REC = " DTCHK004-REC
      *
           .
       2111-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（２）
      *****************************************************************
       212-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 220-REIGAI-CHK-SEC
           IF    ( FLG-REIGAI    =     ZERO )
               PERFORM 2121-ERRMES-HEN-SEC
           END-IF
      *
           .
       212-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（２）
      *****************************************************************
       2121-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 2111-KOHNUM-GET-SEC
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
      *    チェック結果ファイル共通編集項目編集処理
           PERFORM 2111-DTCHK004-KYOTU-HEN-SEC
      *
           IF    ( SRH-KOHNUMMEI       =   SPACE )
               MOVE    WRK-ERRMES      TO  DTCHK004-ERRMSG
           ELSE
               STRING  "［"            DELIMITED   BY  SIZE
                       SRH-KOHNUMMEI   DELIMITED   BY  SPACE
                       "］"            DELIMITED   BY  SIZE
                       WRK-ERRMES      DELIMITED   BY  SPACE
               INTO    DTCHK004-ERRMSG
               END-STRING
           END-IF
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
      *    DISPLAY "DTCHK004-REC = " DTCHK004-REC
      *
           .
       2121-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（３）
      *****************************************************************
       213-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 220-REIGAI-CHK-SEC
           IF    ( FLG-REIGAI    =     ZERO )
               PERFORM 2131-ERRMES-HEN-SEC
           END-IF
      *
           .
       213-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（３）
      *****************************************************************
       2131-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 2111-SRYKA-GET-SEC
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
      *    チェック結果ファイル共通編集項目編集処理
           PERFORM 2111-DTCHK004-KYOTU-HEN-SEC
      *
           IF    ( SRH-1005-SRYKAMEI2  =   SPACE )
               STRING  WRK-ERRMES          DELIMITED   BY  SPACE
                       "【"                DELIMITED   BY  SIZE
                       WRK-ERRMES-PLUS     DELIMITED   BY  SPACE
                       "】"
               INTO    DTCHK004-ERRMSG
               END-STRING
           ELSE
               STRING  "［"                DELIMITED   BY  SIZE
                       SRH-1005-SRYKAMEI2  DELIMITED   BY  SPACE
                       "］"                DELIMITED   BY  SIZE
                       WRK-ERRMES          DELIMITED   BY  SPACE
                       "【"                DELIMITED   BY  SIZE
                       WRK-ERRMES-PLUS     DELIMITED   BY  SPACE
                       "】"
               INTO    DTCHK004-ERRMSG
               END-STRING
           END-IF
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
      *    DISPLAY "DTCHK004-REC = " DTCHK004-REC
      *
           .
       2131-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（４）
      *****************************************************************
       214-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 220-REIGAI-CHK-SEC
           IF    ( FLG-REIGAI    =     ZERO )
               PERFORM 2141-ERRMES-HEN-SEC
           END-IF
      *
           .
       214-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（４）
      *****************************************************************
       2141-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 2111-SRYKA-GET-SEC
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
      *    チェック結果ファイル共通編集項目編集処理
           PERFORM 2111-DTCHK004-KYOTU-HEN-SEC
      *
           IF    ( SRH-1005-SRYKAMEI2  =   SPACE )
               STRING  WRK-ERRMES          DELIMITED   BY  SPACE
               INTO    DTCHK004-ERRMSG
               END-STRING
           ELSE
               STRING  "［"                DELIMITED   BY  SIZE
                       SRH-1005-SRYKAMEI2  DELIMITED   BY  SPACE
                       "］"                DELIMITED   BY  SIZE
                       WRK-ERRMES          DELIMITED   BY  SPACE
               INTO    DTCHK004-ERRMSG
               END-STRING
           END-IF
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
      *    DISPLAY "DTCHK004-REC = " DTCHK004-REC
      *
           .
       2141-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（５）
      *****************************************************************
       215-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 220-REIGAI-CHK-SEC
           IF    ( FLG-REIGAI    =     ZERO )
               PERFORM 2151-ERRMES-HEN-SEC
           END-IF
      *
           .
       215-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（５）
      *****************************************************************
       2151-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
      *    チェック結果ファイル共通編集項目編集処理
           PERFORM 2111-DTCHK004-KYOTU-HEN-SEC
      *
           IF    ( WRK-ERRMES-PLUS     =   SPACE )
               MOVE    WRK-ERRMES      TO  DTCHK004-ERRMSG
           ELSE
               STRING  WRK-ERRMES          DELIMITED   BY  SPACE
                       "【"                DELIMITED   BY  SIZE
                       WRK-ERRMES-PLUS     DELIMITED   BY  SPACE
                       "】"
               INTO    DTCHK004-ERRMSG
               END-STRING
           END-IF
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
      *    DISPLAY "DTCHK004-REC = " DTCHK004-REC
      *
           .
       2151-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（６）
      *****************************************************************
       216-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 220-REIGAI-CHK-SEC
           IF    ( FLG-REIGAI    =     ZERO )
               PERFORM 2161-ERRMES-HEN-SEC
           END-IF
      *
           .
       216-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（６）
      *****************************************************************
       2161-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 2111-HKNNUM-GET-SEC
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
      *    チェック結果ファイル共通編集項目編集処理
           PERFORM 2111-DTCHK004-KYOTU-HEN-SEC
      *
           IF    ( SRH-HKNNUMMEI       =   SPACE )
               STRING  WRK-ERRMES      DELIMITED   BY  SPACE
                   "【"                DELIMITED   BY  SIZE
                   WRK-ERRMES-PLUS     DELIMITED   BY  SPACE
                   "】"
               INTO    DTCHK004-ERRMSG
               END-STRING
           ELSE
               STRING  "［"            DELIMITED   BY  SIZE
                       SRH-HKNNUMMEI   DELIMITED   BY  SPACE
                       "］"            DELIMITED   BY  SIZE
                       WRK-ERRMES      DELIMITED   BY  SPACE
                       "【"            DELIMITED   BY  SIZE
                       WRK-ERRMES-PLUS DELIMITED   BY  SPACE
                       "】"
               INTO    DTCHK004-ERRMSG
               END-STRING
           END-IF
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
      *    DISPLAY "DTCHK004-REC = " DTCHK004-REC
      *
           .
       2161-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（７）
      *****************************************************************
       217-ERRMES-HEN-SEC              SECTION.
      *
           PERFORM 220-REIGAI-CHK-SEC
           IF    ( FLG-REIGAI    =     ZERO )
               PERFORM 2171-ERRMES-HEN-SEC
           END-IF
      *
           .
       217-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理（７）
      *****************************************************************
       2171-ERRMES-HEN-SEC             SECTION.
      *
           PERFORM 2111-ERRMES-GET-SEC
      *
           INITIALIZE  DTCHK004-REC
      *
           MOVE    WRK-ERRCD           TO  DTCHK004-ERRCD
      *
           COMPUTE CNT-CHKERR  =   CNT-CHKERR  +   1
           MOVE    CNT-CHKERR          TO  DTCHK004-ERRCD-SEQ
      *
           MOVE    "Z"                 TO  DTCHK004-HKNKBN
           MOVE    TERRINF-LASTYMD     TO  DTCHK004-LASTYMD
           MOVE    TERRINF-SRYCD       TO  DTCHK004-SRYCD
      *
           MOVE    "1"                 TO  DTCHK004-ERRSBT
           MOVE    "*****"             TO  DTCHK004-PTNUM
           MOVE    DTCHK004-ERRCD      TO  DTCHK004-R1-ERRCD
           MOVE    DTCHK004-ERRCD-SEQ  TO  DTCHK004-R1-ERRCD-SEQ
      *
           STRING  WRK-ERRMES          DELIMITED   BY  SPACE
                  "【"                 DELIMITED   BY  SIZE
                   WRK-ERRMES-PLUS     DELIMITED   BY  SPACE
                   "】"
           INTO    DTCHK004-ERRMSG
           END-STRING
      *
           WRITE   DTCHK004-REC
           COMPUTE CNT-DTCHK004    =   CNT-DTCHK004    +   1
      *
           .
       2171-ERRMES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    例外チェック処理
      *****************************************************************
       220-REIGAI-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-REIGAI
      *
           EVALUATE    TERRINF-CHKKBN
           WHEN    SPACE
      *
                   MOVE    SPACE           TO  WRK-REIGAI-CHKKBN
                   MOVE    WRK-ERRCD (1:2) TO  WRK-REIGAI-CHKNUM
      *
               EVALUATE    TRUE
               WHEN  ( WRK-REIGAI-CHKNUM       =   "20" )
                 AND ( WRK-ERRCD               =   "200001" )
                   MOVE    TERRINF-TANICD  TO  WRK-REIGAI-TANICD
                   MOVE    TERRINF-SRYCD   TO  WRK-REIGAI-CD
                   PERFORM 800-REIGAI-CHK-SEC
               WHEN  ( WRK-REIGAI-CHKNUM       =   "21" )
                   MOVE    TERRINF-SRYCD   TO  WRK-REIGAI-CD
                   PERFORM 800-REIGAI-CHK-SEC
               WHEN    OTHER
                   MOVE    WRK-ERRCD       TO  WRK-REIGAI-CD
                   PERFORM 800-REIGAI-CHK-SEC
               END-EVALUATE
           WHEN    "4"
           WHEN    "5"
           WHEN    "A"
           WHEN    "B"
               MOVE    TERRINF-CHKKBN  TO  WRK-REIGAI-CHKKBN
               MOVE    WRK-ERRCD (1:2) TO  WRK-REIGAI-CHKNUM
               MOVE    TERRINF-SRYCD   TO  WRK-REIGAI-CD
               MOVE    TERRINF-TEKCD   TO  WRK-REIGAI-TEKCD
               PERFORM 800-REIGAI-CHK-SEC
           WHEN    "6"
               MOVE    TERRINF-CHKKBN  TO  WRK-REIGAI-CHKKBN
               MOVE    WRK-ERRCD (1:2) TO  WRK-REIGAI-CHKNUM
               MOVE    TERRINF-SRYCD   TO  WRK-REIGAI-CD
               MOVE    TERRINF-TEKBYOMEI
                                       TO  WRK-REIGAI-TEKCD
               MOVE    TERRINF-TEKCOMPKBN
                                       TO  WRK-REIGAI-COMPKBN
               PERFORM 800-REIGAI-CHK-SEC
           END-EVALUATE
      *
           .
       220-REIGAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ取得処理
      *****************************************************************
       2111-ERRMES-GET-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMES
      *
           PERFORM VARYING IDX-ERRCD   FROM    1   BY  1
                   UNTIL ( IDX-ERRCD   >   ERRMES-MAX )
      *
               IF    ( WRK-ERRCD       =   ERRMES-CD (IDX-ERRCD) )
                   MOVE    ERRMES-MESSAGE (IDX-ERRCD)
                                       TO  WRK-ERRMES
                   MOVE    ERRMES-MAX  TO  IDX-ERRCD
               END-IF
      *
           END-PERFORM
      *
           EVALUATE    WRK-ERRCD
           WHEN    "990019"
               MOVE    WRK-INGAITEN    TO  WRK-STR-LEFT-ZX99
               PERFORM 800-STR-HIDARIZUME-HEN-SEC
               MOVE    WRK-STR-LEFT    TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    "sub"           TO  ORCSSTR-COMMAND
               MOVE    WRK-ERRMES      TO  ORCSSTR-STRING1
               MOVE    "○○○○"      TO  ORCSSTR-STRING2
               MOVE    WRK-OUT-INPUT   TO  ORCSSTR-STRING3
               CALL    "orcsstring" USING  ORCSSTRINGAREA
               MOVE    ORCSSTR-STRING1 TO  WRK-ERRMES
           END-EVALUATE
      *
           .
       2111-ERRMES-GET-EXT.
           EXIT.
      *****************************************************************
      *    チェック結果ファイル共通編集項目編集処理
      *****************************************************************
       2111-DTCHK004-KYOTU-HEN-SEC     SECTION.
      *
           INITIALIZE  DTCHK004-REC
           MOVE    "0"                 TO  DTCHK004-ERRSBT
           MOVE    PTNUM-PTNUM         TO  DTCHK004-PTNUM
           MOVE    WRK-ERRCD           TO  DTCHK004-ERRCD
      *
           COMPUTE CNT-CHKERR  =   CNT-CHKERR  +   1
           MOVE    CNT-CHKERR          TO  DTCHK004-ERRCD-SEQ
      *
           MOVE    DTCHK003-PTID       TO  DTCHK004-PTID
           MOVE    ERRPTINF-NAME       TO  DTCHK004-NAME
           MOVE    ERRPTINF-KANANAME   TO  DTCHK004-KANANAME
           MOVE    DTCHK003-MULHKNKBN  TO  DTCHK004-MULHKNKBN
           MOVE    DTCHK003-HKNKBN     TO  DTCHK004-HKNKBN
           MOVE    ERRPTINF-BIRTHDAY   TO  DTCHK004-BIRTHDAY
           MOVE    ERRPTINF-SEX        TO  DTCHK004-SEX
           MOVE    TERRINF-CHKKBN      TO  DTCHK004-CHKKBN
           MOVE    TERRINF-SRYCD       TO  DTCHK004-SRYCD
           MOVE    TERRINF-SRYKA       TO  DTCHK004-SRYKA
           MOVE    TERRINF-BYOMEI      TO  DTCHK004-BYOMEI
           MOVE    TERRINF-TEKCD       TO  DTCHK004-TEKCD
           MOVE    TERRINF-TEKBYOMEI   TO  DTCHK004-TEKBYOMEI
           MOVE    TERRINF-TEKCOMPKBN  TO  DTCHK004-TEKCOMPKBN
           MOVE    TERRINF-TANICD      TO  DTCHK004-TANICD
           MOVE    TERRINF-TANINAME    TO  DTCHK004-TANINAME
           MOVE    TERRINF-SHONUM      TO  DTCHK004-SHONUM
           MOVE    TERRINF-SRYSTYMD    TO  DTCHK004-SRYSTYMD
           MOVE    TERRINF-SRYEDYMD    TO  DTCHK004-SRYEDYMD
           MOVE    TERRINF-LIMIT       TO  DTCHK004-LIMIT
           MOVE    TERRINF-TSUKISU     TO  DTCHK004-TSUKISU
           MOVE    TERRINF-DAY-G       TO  DTCHK004-DAY-G
           MOVE    TERRINF-LASTYMD     TO  DTCHK004-LASTYMD
      *
           MOVE    HIGH-VALUE          TO  WRK-SRTKBN
           EVALUATE    DTCHK003-HKNKBN
           WHEN  CONST-SYAHO
               MOVE    "1"             TO  WRK-SRTKBN
           WHEN  CONST-KOKUHO
               MOVE    "2"             TO  WRK-SRTKBN
           WHEN  CONST-KOUKI
               MOVE    "3"             TO  WRK-SRTKBN
           WHEN  CONST-ROSAI
               MOVE    "4"             TO  WRK-SRTKBN
           WHEN  CONST-JIBAI
               MOVE    "5"             TO  WRK-SRTKBN
           WHEN  CONST-DAISAN
               MOVE    "6"             TO  WRK-SRTKBN
           WHEN  CONST-KOUGAI
               MOVE    "7"             TO  WRK-SRTKBN
           WHEN  CONST-JIHI
               MOVE    "8"             TO  WRK-SRTKBN
           WHEN  CONST-CHIKEN
               MOVE    "9"             TO  WRK-SRTKBN
           END-EVALUATE
      *
           EVALUATE    ORCBSD01-PRTSRTKBN
           WHEN    "1"
           WHEN    SPACE
               MOVE    DTCHK004-PTNUM      TO  DTCHK004-R1-PTNUM
               MOVE    DTCHK004-PTID       TO  DTCHK004-R1-PTID
               MOVE    WRK-SRTKBN          TO  DTCHK004-R1-SRTKBN
               MOVE    DTCHK004-ERRCD      TO  DTCHK004-R1-ERRCD
               MOVE    DTCHK004-ERRCD-SEQ  TO  DTCHK004-R1-ERRCD-SEQ
           WHEN    "2"
               MOVE    ERRPTINF-KANANAME   TO  DTCHK004-R2-KANANAME
               MOVE    DTCHK004-PTNUM      TO  DTCHK004-R2-PTNUM
               MOVE    DTCHK004-PTID       TO  DTCHK004-R2-PTID
               MOVE    WRK-SRTKBN          TO  DTCHK004-R2-SRTKBN
               MOVE    DTCHK004-ERRCD      TO  DTCHK004-R2-ERRCD
               MOVE    DTCHK004-ERRCD-SEQ  TO  DTCHK004-R2-ERRCD-SEQ
           WHEN    "3"
               MOVE    WRK-SRTKBN          TO  DTCHK004-R3-SRTKBN
               MOVE    DTCHK004-PTNUM      TO  DTCHK004-R3-PTNUM
               MOVE    DTCHK004-PTID       TO  DTCHK004-R3-PTID
               MOVE    DTCHK004-ERRCD      TO  DTCHK004-R3-ERRCD
               MOVE    DTCHK004-ERRCD-SEQ  TO  DTCHK004-R3-ERRCD-SEQ
           WHEN    "4"
               MOVE    WRK-SRTKBN          TO  DTCHK004-R4-SRTKBN
               MOVE    ERRPTINF-KANANAME   TO  DTCHK004-R4-KANANAME
               MOVE    DTCHK004-PTNUM      TO  DTCHK004-R4-PTNUM
               MOVE    DTCHK004-PTID       TO  DTCHK004-R4-PTID
               MOVE    DTCHK004-ERRCD      TO  DTCHK004-R4-ERRCD
               MOVE    DTCHK004-ERRCD-SEQ  TO  DTCHK004-R4-ERRCD-SEQ
           END-EVALUATE
      *
           .
       2111-DTCHK004-KYOTU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類取得処理
      *****************************************************************
       2111-HKNNUM-GET-SEC             SECTION.
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
       2111-HKNNUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    公費の種類取得処理
      *****************************************************************
       2111-KOHNUM-GET-SEC             SECTION.
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
       2111-KOHNUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       2111-SRYKA-GET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SRH-1005-SRYKAMEI1
                                           SRH-1005-SRYKAMEI2
                                           SRH-1005-SRYKAMEI3
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-1005-MAX )
               IF    ( SRH-1005-SRYKA  =   SRH-1005-SRYKAL (SRH-IDX) )
                   MOVE    SRH-1005-SRYKAMEI1L (SRH-IDX)
                                       TO  SRH-1005-SRYKAMEI1
                   MOVE    SRH-1005-SRYKAMEI2L (SRH-IDX)
                                       TO  SRH-1005-SRYKAMEI2
                   MOVE    SRH-1005-SRYKAMEI3L (SRH-IDX)
                                       TO  SRH-1005-SRYKAMEI3
                   MOVE    SRH-1005-MAX
                                       TO  SRH-IDX
               END-IF
           END-PERFORM
      *
           .
       2111-SRYKA-GET-EXT.
           EXIT.
      *****************************************************************
      *    公費負担者番号受給者番号抜けチェック編集処理
      *****************************************************************
       2111-SYS1040-GET-SEC            SECTION.
      *
           MOVE    SPACE               TO  SRH-1040-FTNJANUMCHK
                                           SRH-1040-JKYSNUMCHK
           MOVE    "1"                 TO  SRH-1040-FTNJHANKAKUCHK
                                           SRH-1040-FTNJZENKAKUCHK
                                           SRH-1040-FTNJNUMERICCHK
                                           SRH-1040-JKYSHANKAKUCHK
                                           SRH-1040-JKYSZENKAKUCHK
                                           SRH-1040-JKYSNUMERICCHK
           MOVE    "1"                 TO  SRH-1040-FTNJKETACHK
           MOVE    ZERO                TO  SRH-1040-FTNJKETAMIN
                                           SRH-1040-FTNJKETAMAX
           MOVE    "1"                 TO  SRH-1040-JKYSKETACHK
           MOVE    ZERO                TO  SRH-1040-JKYSKETAMIN
                                           SRH-1040-JKYSKETAMAX
           IF    ( SRH-1040-KOHNUM(1:1)    =  "0" )
      *
               MOVE    "1"             TO  SRH-1040-FTNJHANKAKUCHK
               MOVE    "2"             TO  SRH-1040-FTNJZENKAKUCHK
               MOVE    "1"             TO  SRH-1040-FTNJNUMERICCHK
               MOVE    "1"             TO  SRH-1040-JKYSHANKAKUCHK
               MOVE    "2"             TO  SRH-1040-JKYSZENKAKUCHK
               MOVE    "1"             TO  SRH-1040-JKYSNUMERICCHK
      *
               MOVE    "0"             TO  SRH-1040-FTNJKETACHK
               MOVE     08             TO  SRH-1040-FTNJKETAMIN
               MOVE     08             TO  SRH-1040-FTNJKETAMAX
               MOVE    "0"             TO  SRH-1040-JKYSKETACHK
               MOVE     07             TO  SRH-1040-JKYSKETAMIN
               MOVE     07             TO  SRH-1040-JKYSKETAMAX
           END-IF
      *
           EVALUATE    SRH-1040-KOHNUM
           WHEN    "012"
           WHEN    "025"
               IF    ( SYS-1101-SEIHOJNUMCHKKBN
                                       =   SPACE OR "2" )
                   MOVE    "1"         TO  SRH-1040-JKYSNUMCHK
               END-IF
           WHEN    "030"
                   MOVE    "2"         TO  SRH-1040-JKYSNUMCHK
           END-EVALUATE
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-1040-MAX )
               IF    ( SRH-1040-KOHNUM  =   SRH-1040-KOHNUML (SRH-IDX) )
                   MOVE    SRH-1040-FTNJANUMCHKL (SRH-IDX)
                                       TO  SRH-1040-FTNJANUMCHK
                   MOVE    SRH-1040-JKYSNUMCHKL  (SRH-IDX)
                                       TO  SRH-1040-JKYSNUMCHK
                   MOVE    SRH-1040-FTNJHANKAKUCHKL  (SRH-IDX)
                                       TO  SRH-1040-FTNJHANKAKUCHK
                   MOVE    SRH-1040-FTNJZENKAKUCHKL  (SRH-IDX)
                                       TO  SRH-1040-FTNJZENKAKUCHK
                   MOVE    SRH-1040-FTNJNUMERICCHKL  (SRH-IDX)
                                       TO  SRH-1040-FTNJNUMERICCHK
                   MOVE    SRH-1040-FTNJKETACHKL (SRH-IDX)
                                       TO  SRH-1040-FTNJKETACHK
                   IF    ( SRH-1040-FTNJKETACHK    =   "0" )
                       MOVE    SRH-1040-FTNJKETAMINL (SRH-IDX)
                                       TO  SRH-1040-FTNJKETAMIN-X
                       MOVE    SRH-1040-FTNJKETAMAXL (SRH-IDX)
                                       TO  SRH-1040-FTNJKETAMAX-X
                   ELSE
                       MOVE    ZERO    TO  SRH-1040-FTNJKETAMIN-X
                                           SRH-1040-FTNJKETAMAX-X
                   END-IF
                   MOVE    SRH-1040-JKYSHANKAKUCHKL  (SRH-IDX)
                                       TO  SRH-1040-JKYSHANKAKUCHK
                   MOVE    SRH-1040-JKYSZENKAKUCHKL  (SRH-IDX)
                                       TO  SRH-1040-JKYSZENKAKUCHK
                   MOVE    SRH-1040-JKYSNUMERICCHKL  (SRH-IDX)
                                       TO  SRH-1040-JKYSNUMERICCHK
                   MOVE    SRH-1040-JKYSKETACHKL (SRH-IDX)
                                       TO  SRH-1040-JKYSKETACHK
                   IF    ( SRH-1040-JKYSKETACHK    =   "0" )
                       MOVE    SRH-1040-JKYSKETAMINL (SRH-IDX)
                                       TO  SRH-1040-JKYSKETAMIN-X
                       MOVE    SRH-1040-JKYSKETAMAXL (SRH-IDX)
                                       TO  SRH-1040-JKYSKETAMAX-X
                   ELSE
                       MOVE    ZERO    TO  SRH-1040-JKYSKETAMIN-X
                                           SRH-1040-JKYSKETAMAX-X
                   END-IF
                   MOVE    SRH-1040-MAX
                                       TO  SRH-IDX
               END-IF
           END-PERFORM
      *
           .
       2111-SYS1040-GET-EXT.
           EXIT.
      *****************************************************************
      *    記号抜けチェック処理
      *****************************************************************
       2111-CK4-1102-GET-SEC           SECTION.
      *
           INITIALIZE                  SRH-1102-CHK-DAT
           MOVE    ZERO            TO  FLG-HIT
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-1102-MAX )
                    OR   ( FLG-HIT     NOT =   ZERO )
               IF    ( SRH-1102-JYKNKBNL (SRH-IDX)
                                       =   "0" )
                   IF    ( SRH-1102-HKNJANUML (SRH-IDX)
                                       =   SRH-1102-HKNJANUM )
                           MOVE    1   TO  FLG-HIT
                   END-IF
               ELSE
                   MOVE    SRH-1102-HKNJANUMLENL (SRH-IDX)
                                   TO  WRK-HKNJANUMLEN
                   IF    ( WRK-HKNJANUMLEN >   ZERO )
                       IF    ( SRH-1102-HKNJANUML (SRH-IDX)
                                       =   SRH-1102-HKNJANUM
                                                 (1 : WRK-HKNJANUMLEN))
                        AND  ( SRH-1102-HKNNUML (SRH-IDX)
                                       =   SRH-1102-HKNNUM )
                           MOVE    1   TO  FLG-HIT
                       END-IF
                   ELSE
                       IF    ( SRH-1102-HKNNUML (SRH-IDX)
                                       =   SRH-1102-HKNNUM )
                           MOVE    1   TO  FLG-HIT
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( FLG-HIT         =   1 )
                   IF    ( SRH-1102-UPDDUMMYL (SRH-IDX)
                                       =   SPACE )
                       MOVE    SRH-1102-KIGOCHKL (SRH-IDX)
                                       TO  SRH-1102-KGCHKPTN
      *
                       PERFORM 21111-CK4-1102-DEFAULT-SET-SEC
                   ELSE
                       MOVE    SRH-1102-KGCHKPTNL (SRH-IDX)
                                       TO  SRH-1102-KGCHKPTN
      *
                       MOVE    SRH-1102-KGSUJIL  (SRH-IDX)
                                       TO  SRH-1102-KGSUJI
                       MOVE    SRH-1102-KGKANJIL (SRH-IDX)
                                       TO  SRH-1102-KGKANJI
                       MOVE    SRH-1102-KGABTL   (SRH-IDX)
                                       TO  SRH-1102-KGABT
                       MOVE    SRH-1102-KGHYPNL  (SRH-IDX)
                                       TO  SRH-1102-KGHYPN
                       MOVE    SRH-1102-KGKAKKOL (SRH-IDX)
                                       TO  SRH-1102-KGKAKKO
                       MOVE    SRH-1102-KGETCL   (SRH-IDX)
                                       TO  SRH-1102-KGETC
                       MOVE    SRH-1102-KGKANJINASHIL (SRH-IDX)
                                       TO  SRH-1102-KGKANJINASHI
                       MOVE    SRH-1102-KGKETAMINL (SRH-IDX)
                                       TO  SRH-1102-KGKETAMIN
                       MOVE    SRH-1102-KGKETAMAXL (SRH-IDX)
                                       TO  SRH-1102-KGKETAMAX
      *
                       MOVE    SRH-1102-NM1SUJIL (SRH-IDX)
                                       TO  SRH-1102-NM1SUJI
                       MOVE    SRH-1102-NM1KANJIL(SRH-IDX)
                                       TO  SRH-1102-NM1KANJI
                       MOVE    SRH-1102-NM1ABTL  (SRH-IDX)
                                       TO  SRH-1102-NM1ABT
                       MOVE    SRH-1102-NM1HYPNL (SRH-IDX)
                                       TO  SRH-1102-NM1HYPN
                       MOVE    SRH-1102-NM1KAKKOL(SRH-IDX)
                                       TO  SRH-1102-NM1KAKKO
                       MOVE    SRH-1102-NM1ETCL  (SRH-IDX)
                                       TO  SRH-1102-NM1ETC
                       MOVE    SRH-1102-NMSUJINASHIL (SRH-IDX)
                                       TO  SRH-1102-NMSUJINASHI
                       MOVE    SRH-1102-NMKETAMINL (SRH-IDX)
                                       TO  SRH-1102-NMKETAMIN
                       MOVE    SRH-1102-NMKETAMAXL (SRH-IDX)
                                       TO  SRH-1102-NMKETAMAX
      *
                       MOVE    SRH-1102-NM2SUJIL (SRH-IDX)
                                       TO  SRH-1102-NM2SUJI
                       MOVE    SRH-1102-NM2KANJIL(SRH-IDX)
                                       TO  SRH-1102-NM2KANJI
                       MOVE    SRH-1102-NM2ABTL  (SRH-IDX)
                                       TO  SRH-1102-NM2ABT
                       MOVE    SRH-1102-NM2HYPNL (SRH-IDX)
                                       TO  SRH-1102-NM2HYPN
                       MOVE    SRH-1102-NM2KAKKOL(SRH-IDX)
                                       TO  SRH-1102-NM2KAKKO
                       MOVE    SRH-1102-NM2ETCL  (SRH-IDX)
                                       TO  SRH-1102-NM2ETC
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-HIT     =   ZERO )
               EVALUATE    SRH-1102-HKNNUM
               WHEN    CONST-H039
                   MOVE    KGNMCHK-H039-KGCHECK
                                       TO  SRH-1102-KGCHKPTN
               WHEN    CONST-H009
                   MOVE    KGNMCHK-H009-KGCHECK
                                       TO  SRH-1102-KGCHKPTN
               WHEN    OTHER
                   MOVE    KGNMCHK-HDEF-KGCHECK
                                       TO  SRH-1102-KGCHKPTN
               END-EVALUATE
               PERFORM 21111-CK4-1102-DEFAULT-SET-SEC
           END-IF
      *
           .
       2111-CK4-1102-GET-EXT.
           EXIT.
      *****************************************************************
      *    記号番号チェックデータ初期値設定処理
      *****************************************************************
       21111-CK4-1102-DEFAULT-SET-SEC   SECTION.
      *
           MOVE    KGNMCHK-HDEF-IDX        TO  IDX-KGNM
           EVALUATE    SRH-1102-HKNNUM
           WHEN    "001"
               MOVE    KGNMCHK-H001-IDX    TO  IDX-KGNM
           WHEN    "031"
           WHEN    "032"
           WHEN    "034"
               MOVE    KGNMCHK-H031-IDX    TO  IDX-KGNM
           WHEN    "033"
               MOVE    KGNMCHK-H033-IDX    TO  IDX-KGNM
           WHEN    "006"
           WHEN    "063"
               MOVE    KGNMCHK-H006-IDX    TO  IDX-KGNM
           WHEN    CONST-H039
               MOVE    KGNMCHK-H039-IDX    TO  IDX-KGNM
           WHEN    CONST-H009
               MOVE    KGNMCHK-H009-IDX    TO  IDX-KGNM
           END-EVALUATE
      *
           MOVE    KGNMCHK-KGSUJI(IDX-KGNM)
                                       TO  SRH-1102-KGSUJI
           MOVE    KGNMCHK-KGKANJI(IDX-KGNM)
                                       TO  SRH-1102-KGKANJI
           MOVE    KGNMCHK-KGABT(IDX-KGNM)
                                       TO  SRH-1102-KGABT
           MOVE    KGNMCHK-KGHYPN(IDX-KGNM)
                                       TO  SRH-1102-KGHYPN
           MOVE    KGNMCHK-KGKAKKO(IDX-KGNM)
                                       TO  SRH-1102-KGKAKKO
           MOVE    KGNMCHK-KGETC(IDX-KGNM)
                                       TO  SRH-1102-KGETC
           MOVE    KGNMCHK-KGKANJINASHI(IDX-KGNM)
                                       TO  SRH-1102-KGKANJINASHI
           MOVE    KGNMCHK-KGKETAMIN  (IDX-KGNM)
                                       TO  SRH-1102-KGKETAMIN
           MOVE    KGNMCHK-KGKETAMAX  (IDX-KGNM)
                                       TO  SRH-1102-KGKETAMAX
      *
           MOVE    KGNMCHK-NM1SUJI(IDX-KGNM)
                                       TO  SRH-1102-NM1SUJI
           MOVE    KGNMCHK-NM1KANJI(IDX-KGNM)
                                       TO  SRH-1102-NM1KANJI
           MOVE    KGNMCHK-NM1ABT(IDX-KGNM)
                                       TO  SRH-1102-NM1ABT
           MOVE    KGNMCHK-NM1HYPN(IDX-KGNM)
                                       TO  SRH-1102-NM1HYPN
           MOVE    KGNMCHK-NM1KAKKO(IDX-KGNM)
                                       TO  SRH-1102-NM1KAKKO
           MOVE    KGNMCHK-NM1ETC(IDX-KGNM)
                                       TO  SRH-1102-NM1ETC
           MOVE    KGNMCHK-NMSUJINASHI(IDX-KGNM)
                                       TO  SRH-1102-NMSUJINASHI
           MOVE    KGNMCHK-NMKETAMIN  (IDX-KGNM)
                                       TO  SRH-1102-NMKETAMIN
           MOVE    KGNMCHK-NMKETAMAX  (IDX-KGNM)
                                       TO  SRH-1102-NMKETAMAX
      *
           MOVE    KGNMCHK-NM2SUJI(IDX-KGNM)
                                       TO  SRH-1102-NM2SUJI
           MOVE    KGNMCHK-NM2KANJI(IDX-KGNM)
                                       TO  SRH-1102-NM2KANJI
           MOVE    KGNMCHK-NM2ABT(IDX-KGNM)
                                       TO  SRH-1102-NM2ABT
           MOVE    KGNMCHK-NM2HYPN(IDX-KGNM)
                                       TO  SRH-1102-NM2HYPN
           MOVE    KGNMCHK-NM2KAKKO(IDX-KGNM)
                                       TO  SRH-1102-NM2KAKKO
           MOVE    KGNMCHK-NM2ETC(IDX-KGNM)
                                       TO  SRH-1102-NM2ETC
      *
           .
       21111-CK4-1102-DEFAULT-SET-EXT.
           EXIT.
      *****************************************************************
      *    処理経過編集処理
      *****************************************************************
       20011-SYORI-KEIKA-HEN-SEC        SECTION.
      *
           MOVE    ZERO            TO  WRK-SHO
                                       WRK-AMARI
           DIVIDE  50          INTO    CNT-DTCHK003   GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI       NOT =   ZERO )
               GO  TO  20011-SYORI-KEIKA-HEN-EXT
           END-IF
      *
           MOVE    CNT-DTCHK003    TO  WRK-STR-LEFT-ZX99
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           MOVE    WRK-STR-LEFT    TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT   TO  WRK-KNS-N
           ACCEPT  SYS-TIME        FROM    TIME
           MOVE    SPACE           TO  WRK-MAE-INPUT
           MOVE    SYS-TIME(1:2)   TO  WRK-MAE-INPUT(1:2)
           MOVE    "."             TO  WRK-MAE-INPUT(3:1)
           MOVE    SYS-TIME(3:2)   TO  WRK-MAE-INPUT(4:2)
           MOVE    "."             TO  WRK-MAE-INPUT(6:1)
           MOVE    SYS-TIME(5:2)   TO  WRK-MAE-INPUT(7:2)
      *    PERFORM 800-ZENKAKU-HEN-SEC
      *    MOVE    WRK-OUT-INPUT   TO  WRK-TIME-N
           MOVE    WRK-MAE-INPUT   TO  WRK-TIME-N
      *
      *    ステップ途中処理
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           STRING  "チェック済患者数："    DELIMITED BY SIZE
                   WRK-KNS-N           DELIMITED BY SPACE
                   "人　"              DELIMITED BY SIZE
                   WRK-TIME-N          DELIMITED BY SPACE
           INTO    JOB-YOBI
           END-STRING
      *
           IF      LNK-PRTKANRI-TBL-KEY  NOT =  "RECEPTX"
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    "ORCDTCHK002"       TO  JOB-PGID
               MOVE    "データチェック"    TO  JOB-SHELLMSG
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           .
       20011-SYORI-KEIKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ取得処理
      *****************************************************************
       220-HKNCOMBI-GET-SEC            SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
                                       WRK-HKNCOMBIMEI
                                       WRK-COMB-RSI-HKNKBN
      *
           PERFORM VARYING IDX-COMB    FROM    1   BY  1
                   UNTIL ( IDX-COMB    >   SDTCHK02-O-MAX )
                    OR   ( COMB-HOSPNUM    NOT =   ZERO )
      *
               IF    ( LNK-COMB-HKNCOMBINUM (IDX-COMB)
                                  =    WRK-HKNCOMBI )
                   MOVE    LNK-HKNCOMBI-OCC (IDX-COMB)
                                  TO   HKNCOMBI-REC
                   MOVE    SDTCHK02-O-HKNCOMBIMEI (IDX-COMB)
                                  TO   WRK-HKNCOMBIMEI
                   MOVE    SDTCHK02-O-RSI-HKNKBN  (IDX-COMB)
                                  TO   WRK-COMB-RSI-HKNKBN
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-HKNCOMBIMEI     =   SPACE )
               MOVE    WRK-HKNCOMBI        TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  WRK-HKNCOMBIMEI
           END-IF
      *
           .
       220-HKNCOMBI-GET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ情報取得処理
      *****************************************************************
       230-CHKMST-GET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      TCHK-ACTIVE-G
           MOVE    ZERO                TO  IDX-MAX
      *
           PERFORM 2301-TCHK-CHK-SEC
           IF    ( TCHK-FLG            =   ZERO )
      *
               PERFORM 900-DTCHK018-START-SEC
      *
               PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                        OR   ( IDX-MAX    >=  CONST-TCHK-STK-CD-MAX )
      *
                   MOVE    DTCHK018-REC    TO  CHK-REC
      *
                   MOVE    ZERO            TO  FLG-KIKANCHKCD
      *
                   MOVE    CHK-SRYCD       TO  WRK-LSRYCD
                   MOVE    CHK-CD          TO  WRK-RSRYCD
                   PERFORM 800-CHK-KIKANCHKCD-SEC
                   MOVE    CHK-CD          TO  WRK-LSRYCD
                   MOVE    CHK-SRYCD       TO  WRK-RSRYCD
                   PERFORM 800-CHK-KIKANCHKCD-SEC
                   IF    ( FLG-KIKANCHKCD      =   ZERO )
      *
                       COMPUTE IDX-MAX =   IDX-MAX +   1
      *
                       IF    ( IDX-MAX     =   1 )
                           MOVE    CHK-CHKKBN
                                           TO  TCHK-ACT-CHKKBN
                           MOVE    CHK-SRYCD
                                           TO  TCHK-ACT-SRYCD
                           MOVE    CHK-YUKOSTYMD
                                           TO  TCHK-ACT-YUKOSTYMD
                           MOVE    CHK-YUKOEDYMD
                                           TO  TCHK-ACT-YUKOEDYMD
                           MOVE    CHK-CDKBN
                                               TO  TCHK-ACT-CDKBN
                       END-IF
      *
                       MOVE    CHK-CD      TO  TCHK-ACT-CD (IDX-MAX)
                       MOVE    CHK-NYUGAIKBN
                                           TO  TCHK-ACT-NYUGAIKBN
                                                           (IDX-MAX)
                       MOVE    CHK-NYUGAIBNDKBN
                                           TO  TCHK-ACT-NYUGAIBNDKBN
                                                           (IDX-MAX)
                       MOVE    CHK-CHKERR
                                           TO  TCHK-ACT-CHKERR
                                                           (IDX-MAX)
                       MOVE    CHK-SYORIKBN
                                           TO  TCHK-ACT-SYORIKBN
                                                           (IDX-MAX)
      *
                   END-IF
      *
                   PERFORM 900-DTCHK018-READ-SEC
      *
               END-PERFORM
      *
               IF    ( IDX-MAX         >   ZERO )
                   MOVE    IDX-MAX     TO  TCHK-ACT-CD-MAX
                   PERFORM 2301-TCHK-HEN-SEC
               ELSE
                   MOVE    1           TO  FLG-CHKMST
               END-IF
      *
           END-IF
      *
           .
       230-CHKMST-GET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ退避ワークチェック処理
      *****************************************************************
       2301-TCHK-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TCHK-FLG
      *
           PERFORM VARYING TCHK-IDX    FROM    1   BY  1
                   UNTIL ( TCHK-IDX    >   TCHK-STOCK-MAX )
                    OR   ( TCHK-FLG    =   1 )
      *
               IF    ( TCHK-CHK-CHKKBN =   "8" OR "9" )
                   IF    ( TCHK-STK-CHKKBN    (TCHK-IDX)
                                       =   TCHK-CHK-CHKKBN )
                     AND ( TCHK-STK-ID        (TCHK-IDX)
                                       =   TCHK-CHK-ID )
                       MOVE    1       TO  TCHK-FLG
                   END-IF
               ELSE
                   IF    ( TCHK-STK-CHKKBN    (TCHK-IDX)
                                       =   TCHK-CHK-CHKKBN )
                     AND ( TCHK-STK-SRYCD     (TCHK-IDX)
                                       =   TCHK-CHK-SRYCD )
                     AND ( TCHK-STK-YUKOSTYMD (TCHK-IDX)
                                      <=   TCHK-CHK-YMD )
                     AND ( TCHK-STK-YUKOEDYMD (TCHK-IDX)
                                      >=   TCHK-CHK-YMD )
                     AND ( TCHK-STK-CDKBN     (TCHK-IDX)
                                       =   TCHK-CHK-CDKBN )
                       MOVE    1       TO  TCHK-FLG
                   END-IF
               END-IF
      *
               IF    ( TCHK-FLG        =   1 )
      *
                   INITIALIZE              TCHK-ACTIVE-G
      *
                   MOVE    TCHK-STK-CHKKBN    (TCHK-IDX)
                                       TO  TCHK-ACT-CHKKBN
                   MOVE    TCHK-STK-SRYCD     (TCHK-IDX)
                                       TO  TCHK-ACT-SRYCD
                   MOVE    TCHK-STK-ID        (TCHK-IDX)
                                       TO  TCHK-ACT-ID
                   MOVE    TCHK-STK-YUKOSTYMD (TCHK-IDX)
                                       TO  TCHK-ACT-YUKOSTYMD
                   MOVE    TCHK-STK-YUKOEDYMD (TCHK-IDX)
                                       TO  TCHK-ACT-YUKOEDYMD
                   MOVE    TCHK-STK-CDKBN     (TCHK-IDX)
                                       TO  TCHK-ACT-CDKBN
                   MOVE    TCHK-STK-BYOMEI    (TCHK-IDX)
                                       TO  TCHK-ACT-BYOMEI
                   MOVE    TCHK-STK-CD-G      (TCHK-IDX)
                                       TO  TCHK-ACT-CD-G
      *
               END-IF
      *
           END-PERFORM
      *
           .
       2301-TCHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ退避ワーク編集処理
      *****************************************************************
       2301-TCHK-HEN-SEC               SECTION.
      *
           IF    ( CONST-TCHK-MAX      >   TCHK-STOCK-MAX  )
      *
               COMPUTE TCHK-STOCK-MAX  =   TCHK-STOCK-MAX  +   1
      *
                   MOVE    TCHK-ACT-CHKKBN
                                       TO  TCHK-STK-CHKKBN
                                                       (TCHK-STOCK-MAX)
                   MOVE    TCHK-ACT-SRYCD
                                       TO  TCHK-STK-SRYCD
                                                       (TCHK-STOCK-MAX)
                   MOVE    TCHK-ACT-YUKOSTYMD
                                       TO  TCHK-STK-YUKOSTYMD
                                                       (TCHK-STOCK-MAX)
                   MOVE    TCHK-ACT-YUKOEDYMD
                                       TO  TCHK-STK-YUKOEDYMD
                                                       (TCHK-STOCK-MAX)
                   MOVE    TCHK-ACT-CDKBN
                                       TO  TCHK-STK-CDKBN
                                                       (TCHK-STOCK-MAX)
                   MOVE    TCHK-ACT-BYOMEI
                                       TO  TCHK-STK-BYOMEI
                                                       (TCHK-STOCK-MAX)
                   MOVE    TCHK-ACT-CD-G
                                       TO  TCHK-STK-CD-G
                                                       (TCHK-STOCK-MAX)
      *
           END-IF
      *
           .
       2301-TCHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３情報取得処理
      *****************************************************************
       230-CHKTRD-GET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      TCHK-ACTIVE-G
           MOVE    ZERO                TO  IDX-MAX
      *
           PERFORM 2301-TCHK-CHK-SEC
           IF    ( TCHK-FLG            =   ZERO )
      *
               INITIALIZE                  CHKTRD-REC
               MOVE    TCHK-CHK-CHKKBN TO  CHKTRD-CHKKBN
               MOVE    TCHK-CHK-CDKBN  TO  CHKTRD-CDKBN
               MOVE    TCHK-CHK-SRYCD  TO  CHKTRD-SRYCD
               MOVE    TCHK-CHK-YMD    TO  CHKTRD-YUKOSTYMD
                                           CHKTRD-YUKOEDYMD
               MOVE    WRK-PARA-HOSPNUM TO CHKTRD-HOSPNUM
               MOVE    CHKTRD-REC      TO  MCPDATA-REC
               MOVE    "tbl_chktrd"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  CHKTRD-REC
               ELSE
                   INITIALIZE              CHKTRD-REC
                   MOVE    1           TO  FLG-CHKMST
               END-IF
      *
               PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                        OR   ( IDX-MAX    >=  CONST-TCHK-STK-CD-MAX )
      *
                   COMPUTE IDX-MAX =   IDX-MAX +   1
      *
                   IF    ( IDX-MAX     =   1 )
                       MOVE    CHKTRD-CHKKBN
                                               TO  TCHK-ACT-CHKKBN
                       MOVE    CHKTRD-SRYCD
                                               TO  TCHK-ACT-SRYCD
                       MOVE    CHKTRD-YUKOSTYMD
                                               TO  TCHK-ACT-YUKOSTYMD
                       MOVE    CHKTRD-YUKOEDYMD
                                               TO  TCHK-ACT-YUKOEDYMD
                       MOVE    CHKTRD-CDKBN
                                               TO  TCHK-ACT-CDKBN
                   END-IF
      *
                   MOVE    CHKTRD-CD           TO  TCHK-ACT-CD (IDX-MAX)
                   MOVE    CHKTRD-DAYMONTHKBN  TO  TCHK-ACT-DAYMONTHKBN
                                                               (IDX-MAX)
                   MOVE    CHKTRD-NYUGAIKBN    TO  TCHK-ACT-NYUGAIKBN
                                                               (IDX-MAX)
                   MOVE    CHKTRD-NYUGAIBNDKBN
                                               TO  TCHK-ACT-NYUGAIBNDKBN
                                                               (IDX-MAX)
      *
                   MOVE    "tbl_chktrd"    TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
                   IF    ( MCP-RC          =   ZERO )
                       MOVE    MCPDATA-REC TO  CHKTRD-REC
                   ELSE
                       INITIALIZE              CHKTRD-REC
                       MOVE    1           TO  FLG-CHKMST
                   END-IF
      *
               END-PERFORM
      *
               MOVE    "tbl_chktrd"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               IF    ( IDX-MAX         >   ZERO )
                   MOVE    IDX-MAX     TO  TCHK-ACT-CD-MAX
                   PERFORM 2301-TCHK-HEN-SEC
               ELSE
                   MOVE    1           TO  FLG-CHKMST
               END-IF
      *
           END-IF
      *
           .
       230-CHKTRD-GET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３情報取得処理（特定器材関連手技マスタより）
      *****************************************************************
       231-CHKTRD-GET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-MATERK
      *
           INITIALIZE                      TCHK-ACTIVE-G
           MOVE    ZERO                TO  IDX-MAX
      *
           PERFORM 900-MATERIAL-K-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-MATERK  NOT =   ZERO )
                    OR   ( IDX-MAX        >=   CONST-TCHK-STK-CD-MAX )
      *
               COMPUTE IDX-MAX =   IDX-MAX +   1
      *
               IF    ( IDX-MAX     =   1 )
                   MOVE    "7"                 TO  TCHK-ACT-CHKKBN
                   MOVE    MATERK-SRYCD        TO  TCHK-ACT-SRYCD
                   MOVE    "00000000"          TO  TCHK-ACT-YUKOSTYMD
                   MOVE    "99999999"          TO  TCHK-ACT-YUKOEDYMD
                   MOVE    "2"                 TO  TCHK-ACT-CDKBN
               END-IF
      *
               MOVE    MATERK-SCD              TO  TCHK-ACT-CD (IDX-MAX)
               MOVE    "1"                     TO  TCHK-ACT-DAYMONTHKBN
                                                           (IDX-MAX)
               MOVE    SPACE                   TO  TCHK-ACT-NYUGAIKBN
                                                               (IDX-MAX)
               MOVE    "1"                     TO  TCHK-ACT-NYUGAIBNDKBN
                                                           (IDX-MAX)
      *
               PERFORM 900-MATERIAL-K-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_material_k"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( IDX-MAX         >   ZERO )
               MOVE    IDX-MAX     TO  TCHK-ACT-CD-MAX
               PERFORM 2301-TCHK-HEN-SEC
           ELSE
               MOVE    1           TO  FLG-MATERK
           END-IF
      *
           .
       231-CHKTRD-GET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５情報取得処理
      *****************************************************************
       230-CHK005-GET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      TCHK-ACTIVE-G
           MOVE    ZERO                TO  IDX-MAX
      *
           PERFORM 2301-TCHK-CHK-SEC
           IF    ( TCHK-FLG            =   ZERO )
      *
               INITIALIZE                  CHK005-REC
               MOVE    TCHK-CHK-CHKKBN TO  CHK005-CHKKBN
               MOVE    TCHK-CHK-ID     TO  CHK005-ID
               MOVE    WRK-PARA-HOSPNUM TO CHK005-HOSPNUM
               MOVE    CHK005-REC      TO  MCPDATA-REC
               MOVE    "tbl_chk005"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  CHK005-REC
               ELSE
                   INITIALIZE              CHK005-REC
                   MOVE    1           TO  FLG-CHKMST
               END-IF
      *
               PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                        OR   ( IDX-MAX    >=  CONST-TCHK-STK-CD-MAX )
      *
                   COMPUTE IDX-MAX =   IDX-MAX +   1
      *
                   IF    ( IDX-MAX     =   1 )
                       MOVE    CHK005-CHKKBN
                                               TO  TCHK-ACT-CHKKBN
                       MOVE    CHK005-ID
                                               TO  TCHK-ACT-ID
                       MOVE    CHK005-YUKOSTYMD
                                               TO  TCHK-ACT-YUKOSTYMD
                       MOVE    CHK005-YUKOEDYMD
                                               TO  TCHK-ACT-YUKOEDYMD
                       MOVE    CHK005-CDKBN
                                               TO  TCHK-ACT-CDKBN
                       MOVE    CHK005-BYOMEI   TO  TCHK-ACT-BYOMEI
                   END-IF
      *
                   MOVE    CHK005-SRYCD        TO  TCHK-ACT-CD (IDX-MAX)
                   MOVE    CHK005-DAYMONTHKBN  TO  TCHK-ACT-DAYMONTHKBN
                                                               (IDX-MAX)
                   MOVE    CHK005-NYUGAIKBN    TO  TCHK-ACT-NYUGAIKBN
                                                               (IDX-MAX)
                   MOVE    CHK005-NYUGAIBNDKBN TO  TCHK-ACT-NYUGAIBNDKBN
                                                               (IDX-MAX)
      *
                   MOVE    "tbl_chk005"    TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM 910-DBFETCH-SEC
                   IF    ( MCP-RC          =   ZERO )
                       MOVE    MCPDATA-REC TO  CHK005-REC
                   ELSE
                       INITIALIZE              CHK005-REC
                       MOVE    1           TO  FLG-CHKMST
                   END-IF
      *
               END-PERFORM
      *
               MOVE    "tbl_chk005"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               IF    ( IDX-MAX         >   ZERO )
                   MOVE    IDX-MAX     TO  TCHK-ACT-CD-MAX
                   PERFORM 2301-TCHK-HEN-SEC
               ELSE
                   MOVE    1           TO  FLG-CHKMST
               END-IF
      *
           END-IF
      *
           .
       230-CHK005-GET-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    INPUT   DTCHKERR-FILE
           IF    ( STS-DTCHKERR        =   ZERO )
               CLOSE   DTCHKERR-FILE
           ELSE
               OPEN    OUTPUT              DTCHKERR-FILE
      *
               MOVE    WRK-DTCHKERR    TO  DTCHKERR-REC
               WRITE   DTCHKERR-REC
               CLOSE   DTCHKERR-FILE
      *
             IF    LNK-PRTKANRI-TBL-KEY  NOT =  "RECEPTX"
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-DTCHKERR    TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
             END-IF
           END-IF
      *
           MOVE    1                   TO  FLG-ERR
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( LNK-PRTKANRI-TBL-KEY    =  "RECEPTX" )
               IF    ( CNT-DTCHK004        =  ZERO )
                   MOVE    WKDTCHK003-REC  TO DTCHK003-REC
                   PERFORM 900-PTNUM-KEY-SEL-SEC
                   MOVE    "990011"        TO  WRK-ERRCD
                   INITIALIZE              TERRINF-AREA
                   PERFORM 210-ERRMES-HEN-SEC
               END-IF
           END-IF
      *
           CLOSE                       DTCHK008-FILE
           OPEN    INPUT               DTCHK008-FILE
      *
           IF    ( LNK-PRTKANRI-TBL-KEY    =  "RECEPTX" )
               CONTINUE
           ELSE
               IF    ( ERRKBN-A10001   =   "1" )
                   PERFORM 3001-TEKBYOMEI-MITOUROKU-SEC
               END-IF
           END-IF
      *
           CLOSE                       DCACCT-FILE
                                       DCSRY-FILE
                                       DTCHK003-FILE
                                       DTCHK004-FILE
                                       DTCHK008-FILE
                                       DTCHK010-FILE
                                       DTCHK011-FILE
                                       DTCHK012-FILE
                                       DTCHK013-FILE
                                       DTCHK018-FILE
                                       DTCHK022-FILE
      *
           INITIALIZE                  CHKSND-REC
           MOVE    CHKSND-REC      TO  MCPDATA-REC
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "tmpkey"        TO  MCP-PATHNAME
           PERFORM 910-DBDROP-SEC
      *
           IF      LNK-PRTKANRI-TBL-KEY  NOT =  "RECEPTX"
      *    ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               MOVE    CNT-DTCHK004    TO  JOB-UPDCNT1
               PERFORM 800-ORCSJOB-SEC
      *
               PERFORM 900-DBCOMMIT-SEC
           END-IF
      *
           DISPLAY "*** ORCDTCHK002 END ***"
           DISPLAY "DTCHK004 CNT [" CNT-DTCHK004 "]"
           DISPLAY "***********************"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    適応病名未登録薬剤チェック処理
      *****************************************************************
       3001-TEKBYOMEI-MITOUROKU-SEC            SECTION.
      *
           PERFORM 901-DTCHK008-READ-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK008    NOT =   ZERO )
      *
               IF    ( DTCHK008-CHKKBN     =   "1" )
                AND  ( DTCHK008-CDKBN      =   "1" )
                AND  ( DTCHK008-FORMSTCHK  =   "1" )
                   MOVE    "A1"        TO  WRK-REIGAI-CHKNUM
                   MOVE    "1"         TO  WRK-REIGAI-CHKKBN
                   MOVE    DTCHK008-SRYCD
                                       TO  WRK-REIGAI-CD
                   PERFORM 800-REIGAI-CHK-SEC
                   IF    ( FLG-REIGAI  =   ZERO )
      *
                       MOVE    DTCHK008-SRYCD  TO  WRK-SRYCD
                       MOVE    WRK-KJNYMD      TO  WRK-TNS-YMD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       IF    ( FLG-TENSU       NOT =   ZERO )
                           MOVE    WRK-SRYCD       TO  WRK-MAE-INPUT
                           PERFORM 800-ZENKAKU-HEN-SEC
                           MOVE    WRK-OUT-INPUT   TO  TNS-NAME
                       END-IF
                       IF    ( TNS-YKZKBN  NOT =   3 )
                           MOVE    "A10001"    TO  WRK-ERRCD
                           MOVE    TNS-NAME    TO  WRK-ERRMES-PLUS
                           INITIALIZE                  TERRINF-AREA
                           MOVE    "1"         TO  TERRINF-CHKKBN
                           MOVE    DTCHK008-SRYCD
                                               TO  TERRINF-SRYCD
                           MOVE    WRK-KJNYMD  TO  TERRINF-LASTYMD
                           PERFORM 217-ERRMES-HEN-SEC
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 901-DTCHK008-READ-SEC
      *
           END-PERFORM
      *
           .
       3001-TEKBYOMEI-MITOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    例外設定チェック処理
      *****************************************************************
       800-REIGAI-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-REIGAI
      *
           INITIALIZE                  CHKREIGAI-REC
           MOVE    SPA-HOSPNUM    TO   CHKREIGAI-HOSPNUM
           MOVE    DTCHK003-PTID  TO   CHKREIGAI-PTID
           MOVE    1              TO   CHKREIGAI-RENNUM
           PERFORM 900-DTCHK011-START-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK011    NOT =   ZERO )
                    OR   ( FLG-REIGAI      NOT =   ZERO )
      *
               IF    ( DTCHK011-HOSPNUM    =   SPA-HOSPNUM )
                AND  ( DTCHK011-PTID       =   DTCHK003-PTID )
                AND  ( DTCHK011-NYUGAIKBN  =   ORCBSD01-NYUGAIKBN )
                AND  ( DTCHK011-CHKNUM     =   WRK-REIGAI-CHKNUM )
                   EVALUATE    WRK-REIGAI-CHKKBN
                   WHEN    SPACE
                       EVALUATE    TRUE
                       WHEN  ( WRK-REIGAI-CHKNUM   = "20" )
                         AND ( WRK-ERRCD           = "200001" )
                           IF    ( DTCHK011-SRYCD  = WRK-REIGAI-CD )
                            AND  ( DTCHK011-TANICD = WRK-REIGAI-TANICD )
                               MOVE    1       TO  FLG-REIGAI
                           END-IF
                       WHEN  ( WRK-REIGAI-CHKNUM   = "21" )
                           IF    ( DTCHK011-SRYCD  = WRK-REIGAI-CD )
                               MOVE    1       TO  FLG-REIGAI
                           END-IF
                       WHEN    OTHER
                           IF    ( DTCHK011-ERRCD  = WRK-REIGAI-CD )
                               MOVE    1       TO  FLG-REIGAI
                           END-IF
                       END-EVALUATE
                   WHEN    "4"
                   WHEN    "5"
                   WHEN    "A"
                   WHEN    "B"
                       IF    ( DTCHK011-CHKKBN     = WRK-REIGAI-CHKKBN )
                           IF    ( DTCHK011-SRYCD  = WRK-REIGAI-CD )
                             AND ( DTCHK011-TEKCD  = WRK-REIGAI-TEKCD )
                               MOVE    1   TO  FLG-REIGAI
                           END-IF
                           IF    ( DTCHK011-SRYCD  = WRK-REIGAI-TEKCD )
                             AND ( DTCHK011-TEKCD  = WRK-REIGAI-CD )
                               MOVE    1   TO  FLG-REIGAI
                           END-IF
                       END-IF
                   WHEN    "8"
                   WHEN    "9"
                       IF    ( DTCHK011-CHKKBN   =   WRK-REIGAI-CHKKBN )
                         AND ( DTCHK011-BYOMEI   =   WRK-REIGAI-CD )
                           MOVE    1       TO  FLG-REIGAI
                       END-IF
                   WHEN    "6"
                       IF    ( DTCHK011-CHKKBN     = WRK-REIGAI-CHKKBN )
                           IF    ( DTCHK011-SRYCD  = WRK-REIGAI-CD )
                             AND ( DTCHK011-TEKBYOMEI
                                                   = WRK-REIGAI-TEKCD )
                             AND ( DTCHK011-TEKCOMPKBN
                                                   = WRK-REIGAI-COMPKBN)
                               MOVE    1   TO  FLG-REIGAI
                           END-IF
                       END-IF
                   WHEN    OTHER
                       IF    ( DTCHK011-CHKKBN   =   WRK-REIGAI-CHKKBN )
                         AND ( DTCHK011-SRYCD    =   WRK-REIGAI-CD )
                           MOVE    1       TO  FLG-REIGAI
                       END-IF
                   END-EVALUATE
               END-IF
      *
               PERFORM 900-DTCHK011-READ-SEC
      *
           END-PERFORM
      *
           .
       800-REIGAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    文字列左詰処理
      *****************************************************************
       800-STR-HIDARIZUME-HEN-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-STR-LEFT-FL
      *
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >       500 )
      *
               IF    ( WRK-STR-LEFT (IDXE:1)
                                       NOT =   SPACE )
                   MOVE    WRK-STR-LEFT (IDXE:)
                                           TO  WRK-STR-LEFT-TMP
                   MOVE    WRK-STR-LEFT-TMP
                                           TO  WRK-STR-LEFT
                   MOVE    500             TO  IDXE
               END-IF
      *
           END-PERFORM
      *
           .
       800-STR-HIDARIZUME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-KANA-CHK-SEC                    SECTION.
      *
           MOVE    ZERO                TO  FLG-KANACHK
           MOVE    ZERO                TO  WRK-MAXLEN
      *
           INITIALIZE                  ORCXKANACONVAREA
      *
           IF  ( WRK-KANACHK-MAE-INPUT =   SPACE )
               GO  TO  800-KANA-CHK-EXT
           END-IF
      *
           MOVE    300                 TO  KANACONV-LEN
           MOVE    1000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    14                  TO  KANACONV-CHAR-TYPE
           MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           IF    ( KANACONV-RETURN     =   ZERO )
               MOVE    ZERO            TO  FLG-KANACHK
           ELSE
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "check"         TO  ORCSSTR-COMMAND
               MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  ORCSSTR-STRING1
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               IF    ( ORCSSTR-NUM2    =   ZERO )
                   MOVE    1           TO  FLG-KANACHK
               ELSE
                   MOVE    2           TO  FLG-KANACHK
               END-IF
           END-IF
           MOVE    KANACONV-MAXLEN     TO  WRK-MAXLEN
      *
           .
       800-KANA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       801-KANA-CHK-SEC                    SECTION.
      *
           IF    ( SPA-ENCODING        =   1 )
               PERFORM 800-KANA-CHK-SEC
               MOVE    KANACONV-OUTDATA    TO  WRK-KANACHK-MAE-INPUT
           ELSE
               PERFORM 802-KANA-CHK-SEC
           END-IF
      *
           .
       801-KANA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック（ENCODING ≠ EUC）
      *****************************************************************
       802-KANA-CHK-SEC                    SECTION.
      *
           MOVE    ZERO                TO  FLG-KANACHK
           MOVE    ZERO                TO  WRK-MAXLEN
      *
           INITIALIZE                  ORCXKANACONVAREA
      *
           IF  ( WRK-KANACHK-MAE-INPUT NOT =   SPACE )
      *
               MOVE    300                 TO  KANACONV-LEN
               MOVE    1000                TO  KANACONV-STLEN
               MOVE    0                   TO  KANACONV-CONV-FLAG
               MOVE    14                  TO  KANACONV-CHAR-TYPE
               MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACONV-INDATA
               CALL    "kanaconv"          USING ORCXKANACONVAREA
               IF    ( KANACONV-RETURN     =   ZERO )
                   MOVE    ZERO            TO  FLG-KANACHK
               ELSE
                   MOVE    300             TO  KANACONV-LEN
                   MOVE    1000            TO  KANACONV-STLEN
                   MOVE    0               TO  KANACONV-CONV-FLAG
                   MOVE    14              TO  KANACONV-CHAR-TYPE
                   MOVE    KANACONV-OUTDATA
                                           TO  KANACONV-INDATA
                   CALL    "kanaconv"      USING ORCXKANACONVAREA
                   IF    ( KANACONV-RETURN NOT =   ZERO )
                       MOVE    1           TO  FLG-KANACHK
                   END-IF
      *
                   MOVE    300             TO  KANACONV-LEN
                   MOVE    1000            TO  KANACONV-STLEN
                   MOVE    0               TO  KANACONV-CONV-FLAG
                   MOVE    256             TO  KANACONV-CHAR-TYPE
                   MOVE    WRK-KANACHK-MAE-INPUT
                                           TO  KANACONV-INDATA
                   CALL    "kanaconv"          USING ORCXKANACONVAREA
      *
               END-IF
      *
               MOVE    KANACONV-OUTDATA    TO  WRK-KANACHK-MAE-INPUT
               MOVE    KANACONV-MAXLEN     TO  WRK-MAXLEN
      *
           END-IF
      *
           .
       802-KANA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角カタカナ入力チェック
      *****************************************************************
       800-KATAKANA-CHK-SEC                    SECTION.
      *
           MOVE    ZERO                TO  FLG-KANACHK
           MOVE    ZERO                TO  WRK-MAXLEN
      *
           INITIALIZE                  ORCXKANACONVAREA
      *
           IF  ( WRK-KANACHK-MAE-INPUT =   SPACE )
               GO  TO  800-KATAKANA-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX-KANA    FROM    1   BY  2
                   UNTIL ( IDX-KANA    >   CONST-INPUT-MAX )
                    OR   ( WRK-KANACHK-MAE-INPUT (IDX-KANA : )
                                       =   SPACE )
      *
               IF    ( WRK-KANACHK-MAE-INPUT (IDX-KANA : 2 )
                                   =   "　" OR "ー" OR "゛" OR "゜" )
      *
                   MOVE    "ン"
                           TO  WRK-KANACHK-MAE-INPUT (IDX-KANA : 2 )
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    300                 TO  KANACONV-LEN
           MOVE    1000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    4                   TO  KANACONV-CHAR-TYPE
           MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           MOVE    KANACONV-RETURN     TO  FLG-KANACHK
           MOVE    KANACONV-MAXLEN     TO  WRK-MAXLEN
      *
           .
       800-KATAKANA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角カタカナ入力チェック（労災レセ電）
      *****************************************************************
       801-KATAKANA-CHK-SEC                    SECTION.
      *
           MOVE    ZERO                TO  FLG-KANACHK
           MOVE    SPACE               TO  WRK-ERR-KANA
      *
           IF  ( WRK-KANACHK-MAE-INPUT =   SPACE )
               CONTINUE
           ELSE
               PERFORM VARYING IDX-KANA    FROM    1   BY  2
                       UNTIL ( IDX-KANA    >   CONST-INPUT-MAX )
                        OR   ( WRK-KANACHK-MAE-INPUT (IDX-KANA : )
                                           =   SPACE )
      *
                   MOVE    ZERO        TO  FLG-RSI-KANA
                   PERFORM VARYING IDXR    FROM    1   BY  1
                           UNTIL ( IDXR    >   CONST-RSI-KATAKANA-MAX )
                            OR   ( FLG-RSI-KANA
                                           NOT =   ZERO )
      *
                       IF    ( WRK-KANACHK-MAE-INPUT (IDX-KANA : 2)
                                       =   CONST-RSI-KATAKANA (IDXR))
                           MOVE    1   TO  FLG-RSI-KANA
                       END-IF
      *
                   END-PERFORM
      *
                   IF    ( FLG-RSI-KANA    =   ZERO )
                       MOVE    "■"    TO  WRK-ERR-KANA
                                                     (IDX-KANA : 2)
                       MOVE    1       TO  FLG-KANACHK
                   ELSE
                       MOVE    WRK-KANACHK-MAE-INPUT (IDX-KANA : 2)
                                       TO  WRK-ERR-KANA
                                                     (IDX-KANA : 2)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       801-KATAKANA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    半角ェック
      *****************************************************************
       800-HANKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-KANACHK
           MOVE    ZERO                TO  WRK-MAXLEN
      *
           INITIALIZE                  ORCXKANACONVAREA
      *
           IF  ( WRK-KANACHK-MAE-INPUT =   SPACE )
               GO  TO  800-HANKAKU-CHK-EXT
           END-IF
      *
           MOVE    300                 TO  KANACONV-LEN
           MOVE    1000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    112                 TO  KANACONV-CHAR-TYPE
           MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           MOVE    KANACONV-RETURN     TO  FLG-KANACHK
           MOVE    KANACONV-MAXLEN     TO  WRK-MAXLEN
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角ェック
      *****************************************************************
       800-ZENKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-KANACHK
           MOVE    ZERO                TO  WRK-MAXLEN
      *
           INITIALIZE                  ORCXKANACONVAREA
      *
           IF  ( WRK-KANACHK-MAE-INPUT =   SPACE )
               GO  TO  800-ZENKAKU-CHK-EXT
           END-IF
      *
           MOVE    300                 TO  KANACONV-LEN
           MOVE    1000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    15                  TO  KANACONV-CHAR-TYPE
           MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           IF    ( KANACONV-RETURN     =   ZERO )
               MOVE    ZERO            TO  FLG-KANACHK
           ELSE
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "check"         TO  ORCSSTR-COMMAND
               MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  ORCSSTR-STRING1
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               IF    ( ORCSSTR-NUM2    =   ZERO )
                   MOVE    1           TO  FLG-KANACHK
               ELSE
                   MOVE    2           TO  FLG-KANACHK
               END-IF
           END-IF
           MOVE    KANACONV-MAXLEN     TO  WRK-MAXLEN
      *
           .
       800-ZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-HEN-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-OUT-INPUT
           MOVE    ZERO            TO  WRK-OUT-MAX
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE 300                TO  KANACONV-LEN
           MOVE 1000               TO  KANACONV-STLEN
           MOVE 1                  TO  KANACONV-CONV-FLAG
           MOVE 15                 TO  KANACONV-CHAR-TYPE
           MOVE WRK-MAE-INPUT      TO  KANACONV-INDATA
           CALL "kanaconv"             USING ORCXKANACONVAREA
           IF    ( KANACONV-RETURN =   ZERO )
               MOVE    KANACONV-OUTDATA
                                       TO  WRK-OUT-INPUT
               MOVE    KANACONV-MAXLEN TO  WRK-OUT-MAX
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
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           IF    ( LNK-PRTKANRI-TBL-KEY    NOT =  "RECEPTX" )
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA 
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    期間判定診療行為コードチェック処理
      *****************************************************************
       800-CHK-KIKANCHKCD-SEC          SECTION.
      *
           PERFORM VARYING IDX-KANRIRYO    FROM    1   BY  1
                   UNTIL ( IDX-KANRIRYO    >   CONST-TSANTEICD-MAX )
                    OR   ( FLG-KIKANCHKCD  =   1 )
               PERFORM VARYING IDX35   FROM    1   BY  1
                       UNTIL ( IDX35       >   CONST-TSANTEICD-MAX )
                        OR   ( FLG-KIKANCHKCD  =   1 )
      *
                   IF    ( WRK-LSRYCD      =   CONST-TSANTEICD-SRYCD
                                                      (IDX-KANRIRYO))
                     AND ( CONST-TSANTEICD-TYPE (IDX-KANRIRYO)(1:1)
                                           =       "R" )
                     AND ( WRK-RSRYCD      =   CONST-TSANTEICD-SRYCD
                                                      (IDX35))
                     AND ( CONST-TSANTEICD-TYPE (IDX35)
                                           =       "CM"
                                                OR "CR"
                                                OR "CS"
                                                OR "CN"
                                                OR "CU"
                                                OR "CK"  )
                       MOVE    1           TO  FLG-KIKANCHKCD
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           .
      *
       800-CHK-KIKANCHKCD-EXT.
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
      *    一時チェックマスタ２編集処理
      *****************************************************************
       800-TMPSND-EDIT-SEC             SECTION.
      *
           INITIALIZE                      SS013-AREA
           MOVE     "DBINSERT"         TO  SS013-FUNC
           MOVE     WRK-CHK-CHKKBN     TO  SS013-CHKKBN
           MOVE     WRK-CHK-SRYCD      TO  SS013-SRYCD
           MOVE     WRK-KJNYMD         TO  SS013-KJNYMD
           MOVE     CONST-TEMP         TO  SS013-TEMP-NAME
           MOVE     SMCNDATE-YMD       TO  SS013-TEMP-KEY
           MOVE     SMCNDATE-HMS       TO  SS013-TEMP-KEY(9:)
           CALL     "ORCSS013"         USING
                                           SS013-AREA
                                           SPA-AREA
           
      *
           .
       800-TMPSND-EDIT-EXT.
            EXIT.
      *****************************************************************
      *    一時ファイル名取得
      *****************************************************************
       800-ORCSGETTEMP-SEC             SECTION.
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    ASGNPARA        TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME TO ASGNPARA
      *
           .
       800-ORCSGETTEMP-EXT.
            EXIT.
      *****************************************************************
      *    対象患者ファイル読込処理
      *****************************************************************
       900-DTCHK003-READ-SEC           SECTION.
      *
           READ    DTCHK003-FILE
           AT  END
               MOVE    1               TO  FLG-DTCHK003
           END-READ
           IF    ( STS-COMMON  >   10 )
               MOVE    1               TO  FLG-DTCHK003
           END-IF
      *
           IF    ( FLG-DTCHK003        =   ZERO )
               COMPUTE CNT-DTCHK003    =   CNT-DTCHK003    +   1
               IF    ( CNT-DTCHK003    =   1 )
                   MOVE    DTCHK003-REC    TO  WKDTCHK003-REC
               END-IF
           END-IF
      *
           .
       900-DTCHK003-READ-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理(CHK-KEY5)
      *****************************************************************
       900-CHK-KEY5-SEL-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      CHK-REC
           MOVE    WRK-CHK-CHKKBN      TO  CHK-CHKKBN
           MOVE    WRK-CHK-CDKBN       TO  CHK-CDKBN
           MOVE    WRK-KJNYMD          TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM    TO  CHK-HOSPNUM
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-CHK-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       901-CHKSND-KEY31-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      CHKSND-REC
           MOVE    WRK-CHK-CHKKBN      TO  CHKSND-CHKKBN
           MOVE    "%"                 TO  CHKSND-SRYCD
           MOVE    WRK-CHK-CDKBN       TO  CHKSND-CDKBN
           MOVE    WRK-KJNYMD          TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM    TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key31"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               INITIALIZE                  CHKSND-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key31"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       901-CHKSND-KEY31-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理(CHKTRD-KEY5)
      *****************************************************************
       900-CHKTRD-KEY5-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      CHKTRD-REC
           MOVE    WRK-CHK-CHKKBN      TO  CHKTRD-CHKKBN
           MOVE    WRK-CHK-CDKBN       TO  CHKTRD-CDKBN
           MOVE    WRK-KJNYMD          TO  CHKTRD-YUKOSTYMD
                                           CHKTRD-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM    TO  CHKTRD-HOSPNUM
           MOVE    CHKTRD-REC          TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
           ELSE
               INITIALIZE                  CHKTRD-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
      *
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-CHKTRD-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理(CHK005-KEY5)
      *****************************************************************
       900-CHK005-KEY5-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      CHK005-REC
           MOVE    WRK-CHK-CHKKBN      TO  CHK005-CHKKBN
           MOVE    WRK-CHK-CDKBN       TO  CHK005-CDKBN
           MOVE    WRK-KJNYMD          TO  CHK005-YUKOSTYMD
                                           CHK005-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM    TO  CHK005-HOSPNUM
           MOVE    CHK005-REC          TO  MCPDATA-REC
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK005-REC
           ELSE
               INITIALIZE                  CHK005-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
      *
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-CHK005-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY3-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
      *
       900-CHKSND-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２FETCH処理
      *****************************************************************
       900-CHKSND-KEY3-FET-SEC      SECTION.
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
      *
       900-CHKSND-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       901-CHKSND-KEY3-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-CHKSND
      *
           MOVE    CHKSND-REC      TO  MCPDATA-REC
      *
           PERFORM 900-DTCHK010-START-SEC
           IF    ( FLG-DTCHK010    =   ZERO )
               MOVE    DTCHK010-DAT    TO  CHKSND-REC
           ELSE
               MOVE    "tbl_chksnd"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  CHKSND-REC
                   MOVE    CHKSND-REC  TO  DTCHK010-DAT
                   WRITE   DTCHK010-REC
               ELSE
                   MOVE    1           TO  FLG-CHKSND
                   INITIALIZE              CHKSND-REC
               END-IF
      *
               MOVE    "tbl_chksnd"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
      *
       901-CHKSND-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY31-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key31"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
      *
       900-CHKSND-KEY31-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２FETCH処理
      *****************************************************************
       900-CHKSND-KEY31-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key31"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
      *
       900-CHKSND-KEY31-FET-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル読込処理
      *****************************************************************
       900-DTCHK008-WRITE-SEC          SECTION.
      *
           INITIALIZE                  DTCHK008-KEY
           MOVE    WRK-CHK-CHKKBN      TO  DTCHK008-CHKKBN
           MOVE    WRK-CHK-CDKBN       TO  DTCHK008-CDKBN
           MOVE    WRK-CHK-SRYCD       TO  DTCHK008-SRYCD
      *
           WRITE   DTCHK008-REC
      *
           .
       900-DTCHK008-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル読込処理
      *****************************************************************
       901-DTCHK008-WRITE-SEC          SECTION.
      *
           INITIALIZE                  DTCHK008-KEY
           MOVE    WRK-CHK-CHKKBN      TO  DTCHK008-CHKKBN
           MOVE    WRK-CHK-CDKBN       TO  DTCHK008-CDKBN
           MOVE    WRK-CHK-SRYCD       TO  DTCHK008-SRYCD
           MOVE    "1"                 TO  DTCHK008-FORMSTCHK
      *
           WRITE   DTCHK008-REC
      *
           .
       901-DTCHK008-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル読込処理
      *****************************************************************
       900-DTCHK008-READ-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-DTCHK008
      *
           INITIALIZE                  DTCHK008-KEY
           MOVE    WRK-CHK-CHKKBN      TO  DTCHK008-CHKKBN
           MOVE    WRK-CHK-CDKBN       TO  DTCHK008-CDKBN
           MOVE    WRK-CHK-SRYCD       TO  DTCHK008-SRYCD
           READ    DTCHK008-FILE
           INVALID KEY
               MOVE    1               TO  FLG-DTCHK008
           END-READ
      *
           .
       900-DTCHK008-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル読込処理
      *****************************************************************
       901-DTCHK008-READ-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-DTCHK008
      *
           READ    DTCHK008-FILE       NEXT
           AT  END
               MOVE    1               TO  FLG-DTCHK008
           END-READ
           IF    ( STS-COMMON  >   10 )
               MOVE    1               TO  FLG-DTCHK008
           END-IF
      *
           .
       901-DTCHK008-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル読込処理
      *****************************************************************
       900-DTCHK009-READ-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-DTCHK009
      *
           READ    DTCHK009-FILE
           INVALID KEY
               MOVE    1               TO  FLG-DTCHK009
           END-READ
      *
           .
       900-DTCHK009-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療会計START処理
      *****************************************************************
       900-DCACCT-START-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-DCACCT
      *
           INITIALIZE                  DCACCT-KEY
           MOVE    DTCHK003-PTID       TO  DCACCT-PTID
      *
           START   DCACCT-FILE
               KEY IS  >=   DCACCT-KEY
           INVALID
               MOVE    1               TO  FLG-DCACCT
           NOT INVALID
               PERFORM 900-DCACCT-READ-SEC
           END-START
      *
           .
       900-DCACCT-START-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療会計読込処理
      *****************************************************************
       900-DCACCT-READ-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-DCACCT      NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               READ    DCACCT-FILE     NEXT
               AT  END
                       MOVE    1           TO  FLG-DCACCT
               NOT AT  END
      *
                   IF    ( DTCHK003-PTID   NOT =   DCACCT-PTID )
                       MOVE    1           TO  FLG-DCACCT
                   ELSE
      *
                       MOVE    1               TO  FLG-LOOP-END
      *
                       IF    ( DCACCT-STS-NYUINDAY =   ZERO )
      *
                           IF    ( DCACCT-ZAIKAISU =   ZERO )
                               MOVE   ZERO    TO   FLG-LOOP-END
                           END-IF
      *
                           IF    ( DCACCT-SRYKBN   =   "95" OR "96"
                                                    OR "98" )
                               MOVE   ZERO    TO   FLG-LOOP-END
                           END-IF
      *
                           IF    ( DCACCT-ZAIKBN   =   1    OR 2 )
                               MOVE   ZERO    TO   FLG-LOOP-END
                           END-IF
      *
                           MOVE    ZERO       TO   FLG-ZAIARI
                           PERFORM VARYING IDX-PTSRY FROM 1 BY 1
                                   UNTIL ( IDX-PTSRY   >   PTSRY-MAX )
                                    OR   ( FLG-ZAIARI  =   1 )
                               IF   (  DCACCT-ZAINUM
                                       =   PTSRY-ZAINUM (IDX-PTSRY))
                                AND (  DCACCT-ACCTKBN
                                       =   PTSRY-ACCTKBN(IDX-PTSRY))
                                   MOVE    1  TO   FLG-ZAIARI
                               END-IF
                           END-PERFORM
      *
                           IF    ( FLG-ZAIARI =    ZERO )
                               MOVE    ZERO   TO   FLG-LOOP-END
                           END-IF
      *
                       END-IF
                   END-IF
               END-READ
               IF    ( STS-COMMON  >   10 )
                       MOVE    1           TO  FLG-DCACCT
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-DCACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療会計START処理（禁忌チェック）
      *****************************************************************
       910-DCACCT-START-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-DCACCT
      *
           INITIALIZE                  DCACCT-KEY
           MOVE    DTCHK003-PTID       TO  DCACCT-PTID
      *
           START   DCACCT-FILE
               KEY IS  >=   DCACCT-KEY
           INVALID
               MOVE    1               TO  FLG-DCACCT
           NOT INVALID
               PERFORM 910-DCACCT-READ-SEC
           END-START
      *
           .
       910-DCACCT-START-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療会計読込処理（禁忌チェック）
      *****************************************************************
       910-DCACCT-READ-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-DCACCT      NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               READ    DCACCT-FILE     NEXT
               AT  END
                       MOVE    1           TO  FLG-DCACCT
               NOT AT  END
      *
                   IF    ( DTCHK003-PTID   NOT =   DCACCT-PTID )
                       MOVE    1           TO  FLG-DCACCT
                   ELSE
      *
                       MOVE    1               TO  FLG-LOOP-END
      *
                       IF    ( DCACCT-STS-NYUINDAY =   ZERO )
      *
                           IF    ( DCACCT-ZAIKAISU =   ZERO )
                               MOVE   ZERO    TO   FLG-LOOP-END
                           END-IF
      *
                           MOVE    ZERO       TO   FLG-ZAIARI
                           PERFORM VARYING IDX-PTSRY FROM 1 BY 1
                                   UNTIL ( IDX-PTSRY   >   PTSRY-MAX )
                                    OR   ( FLG-ZAIARI  =   1 )
                               IF   (  DCACCT-ZAINUM
                                       =   PTSRY-ZAINUM (IDX-PTSRY))
                                AND (  DCACCT-ACCTKBN
                                       =   PTSRY-ACCTKBN(IDX-PTSRY))
                                   MOVE    1  TO   FLG-ZAIARI
                               END-IF
                           END-PERFORM
      *
                           IF    ( FLG-ZAIARI =    ZERO )
                               MOVE    ZERO   TO   FLG-LOOP-END
                           END-IF
      *
                       ELSE
                               MOVE   ZERO    TO   FLG-LOOP-END
                       END-IF
      *
                       IF    ( DCACCT-ZAIKBN   =   1    OR 2 )
                               MOVE   ZERO    TO   FLG-LOOP-END
                       END-IF
                   END-IF
               END-READ
               IF    ( STS-COMMON  >   10 )
                   MOVE    1               TO  FLG-DCACCT
               END-IF
      *
           END-PERFORM
      *
           .
      *
       910-DCACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療行為START処理
      *****************************************************************
       900-DCSRY-START-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-DCSRY
      *
           INITIALIZE                  DCSRY-KEY
           MOVE    DTCHK003-PTID       TO  DCSRY-PTID
      *
           START   DCSRY-FILE
               KEY IS  >=   DCSRY-KEY
           INVALID
               MOVE    1               TO  FLG-DCSRY
           NOT INVALID
               PERFORM 900-DCSRY-READ-SEC
           END-START
      *
           .
       900-DCSRY-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療行為読込処理
      *****************************************************************
       900-DCSRY-READ-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-DCSRY       NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               READ    DCSRY-FILE     NEXT
               AT  END
                       MOVE    1           TO  FLG-DCSRY
               NOT AT  END
                   IF    ( DTCHK003-PTID   NOT =   DCSRY-PTID )
                       MOVE    1           TO  FLG-DCSRY
                   ELSE
                       MOVE    1           TO  FLG-LOOP-END
                   END-IF
      *
                   IF    ( DCSRY-SRYSYUKBN (1:2)   =   "99" )
                    AND  ( DCSRY-SRYCD (1:4)   =   "0085" OR "0086" )
                       MOVE    ZERO        TO  FLG-LOOP-END
                   END-IF
      *
               END-READ
               IF    ( STS-COMMON  >   10 )
                   MOVE    1               TO  FLG-DCSRY
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-DCSRY-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療行為START処理
      *****************************************************************
       901-DCSRY-START-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-DCSRY
      *
           INITIALIZE                  DCSRY-KEY
           MOVE    DTCHK003-PTID       TO  DCSRY-PTID
      *
           START   DCSRY-FILE
               KEY IS  >=   DCSRY-KEY
           INVALID
               MOVE    1               TO  FLG-DCSRY
           NOT INVALID
               PERFORM 901-DCSRY-READ-SEC
           END-START
      *
           .
       901-DCSRY-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療行為読込処理
      *    （チェックマスタ突合せチェック用にコメントデータ取得）
      *****************************************************************
       901-DCSRY-READ-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-DCSRY       NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               READ    DCSRY-FILE     NEXT
               AT  END
                       MOVE    1           TO  FLG-DCSRY
               NOT AT  END
                   IF    ( DTCHK003-PTID   NOT =   DCSRY-PTID )
                       MOVE    1           TO  FLG-DCSRY
                   ELSE
                       MOVE    1           TO  FLG-LOOP-END
                   END-IF
      *
                   IF    ( DCSRY-SRYSYUKBN (1:2)   =   "99" )
                    AND  ( DCSRY-SRYCD (1:4)   =   "0085" OR "0086" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOP-END
                   END-IF
      *
               END-READ
               IF    ( STS-COMMON  >   10 )
                   MOVE    1               TO  FLG-DCSRY
               END-IF
      *
           END-PERFORM
      *
           .
      *
       901-DCSRY-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療行為START処理
      *****************************************************************
       900-DCSANTEI-START-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-DCSANTEI
      *
           INITIALIZE                  DCSANTEI-KEY
           MOVE    DTCHK003-HOSPNUM TO DCSANTEI-HOSPNUM
           MOVE    DTCHK003-PTID   TO  DCSANTEI-PTID
      *
           START   DCSANTEI-FILE
               KEY IS  >=   DCSANTEI-KEY
           INVALID
               MOVE    1           TO  FLG-DCSANTEI
           NOT INVALID
               PERFORM 900-DCSANTEI-READ-SEC
           END-START
      *
           .
       900-DCSANTEI-START-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療行為読込処理
      *****************************************************************
       900-DCSANTEI-READ-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DCSANTEI
      *
           READ    DCSANTEI-FILE   NEXT
           AT  END
                   MOVE    1       TO  FLG-DCSANTEI
           NOT AT  END
      *
               IF    ( DTCHK003-HOSPNUM NOT =  DCSANTEI-HOSPNUM )
                OR   ( DTCHK003-PTID   NOT =   DCSANTEI-PTID )
                   MOVE    1       TO  FLG-DCSANTEI
               END-IF
           END-READ
           IF    ( STS-COMMON  >   10 )
               MOVE    1           TO  FLG-DCSANTEI
           END-IF
      *
           .
      *
       900-DCSANTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２START処理
      *****************************************************************
       900-DTCHK010-START-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK010
      *
           MOVE    CHKSND-REC      TO  DTCHK010-DAT
                                       KEYCHKSND-REC
      *
           MOVE    ZERO            TO  DTCHK010-YUKOSTYMD
                                       DTCHK010-YUKOEDYMD
      *
           START   DTCHK010-FILE
               KEY IS  >=   DTCHK010-KEY
           INVALID
               MOVE    1           TO  FLG-DTCHK010
           NOT INVALID
               PERFORM 900-DTCHK010-READ-SEC
           END-START
      *
           .
       900-DTCHK010-START-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療行為読込処理
      *****************************************************************
       900-DTCHK010-READ-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK010
      *
           READ    DTCHK010-FILE   NEXT
           AT  END
                   MOVE    1       TO  FLG-DTCHK010
           NOT AT  END
      *
               IF    ( KEYCHKSND-HOSPNUM   NOT =   DTCHK010-HOSPNUM )
                OR   ( KEYCHKSND-CHKKBN    NOT =   DTCHK010-CHKKBN )
                OR   ( KEYCHKSND-SRYCD     NOT =   DTCHK010-SRYCD )
                OR   ( KEYCHKSND-CDKBN     NOT =   DTCHK010-CDKBN )
                   MOVE    1       TO  FLG-DTCHK010
               END-IF
           END-READ
           IF    ( STS-COMMON  >   10 )
               MOVE    1           TO  FLG-DTCHK010
           END-IF
      *
           .
       900-DTCHK010-READ-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２START処理
      *****************************************************************
       901-DTCHK010-START-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK010
      *
           MOVE    CHKSND-REC      TO  DTCHK010-DAT
                                       KEYCHKSND-REC
      *
           MOVE    ZERO            TO  DTCHK010-YUKOSTYMD
                                       DTCHK010-YUKOEDYMD
      *
           START   DTCHK010-FILE
               KEY IS  >=   DTCHK010-KEY
           INVALID
               MOVE    1           TO  FLG-DTCHK010
           NOT INVALID
               PERFORM 901-DTCHK010-READ-SEC
           END-START
      *
           .
       901-DTCHK010-START-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療行為読込処理
      *****************************************************************
       901-DTCHK010-READ-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK010
      *
           READ    DTCHK010-FILE   NEXT
           AT  END
                   MOVE    1       TO  FLG-DTCHK010
           NOT AT  END
      *
               IF    ( DTCHK010-HOSPNUM     NOT = KEYCHKSND-HOSPNUM )
                OR   ( DTCHK010-CHKKBN      NOT = KEYCHKSND-CHKKBN )
                OR   ( DTCHK010-SRYCD (1:1) NOT = "P" )
                OR   ( DTCHK010-CDKBN       NOT = KEYCHKSND-CDKBN )
                   MOVE    1       TO  FLG-DTCHK010
               END-IF
           END-READ
           IF    ( STS-COMMON  >   10 )
                   MOVE    1       TO  FLG-DTCHK010
           END-IF
      *
           .
       901-DTCHK010-READ-EXT.
           EXIT.
      *****************************************************************
      *    チェック例外マスタSTART処理
      *****************************************************************
       900-DTCHK011-START-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK011
      *
           MOVE    CHKREIGAI-REC   TO  DTCHK011-REC
                                       KEYREIGAI-REC
      *
           START   DTCHK011-FILE
               KEY IS  >=   DTCHK011-KEY
           INVALID
               MOVE    1           TO  FLG-DTCHK011
           NOT INVALID
               PERFORM 900-DTCHK011-READ-SEC
           END-START
      *
           .
       900-DTCHK011-START-EXT.
           EXIT.
      *****************************************************************
      *    チェック例外マスタ読込処理
      *****************************************************************
       900-DTCHK011-READ-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK011
      *
           READ    DTCHK011-FILE   NEXT
           AT  END
                   MOVE    1       TO  FLG-DTCHK011
           NOT AT  END
      *
               IF    ( KEYREIGAI-HOSPNUM   NOT =   DTCHK011-HOSPNUM )
                OR   ( KEYREIGAI-PTID      NOT =   DTCHK011-PTID )
                   MOVE    1       TO  FLG-DTCHK011
               END-IF
           END-READ
           IF    ( STS-COMMON  >   10 )
                   MOVE    1       TO  FLG-DTCHK011
           END-IF
      *
           .
       900-DTCHK011-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル（病名）読込処理
      *****************************************************************
       900-DTCHK012-READ-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK012
      *
           READ    DTCHK012-FILE
           INVALID
               MOVE    1           TO  FLG-DTCHK012
           END-READ
      *
           .
       900-DTCHK012-READ-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（算定回数）チェックファイルSTART処理
      *****************************************************************
       900-DTCHK013-START-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-DTCHK013
      *
           INITIALIZE                      DTCHK013-REC
           MOVE    DTCHK003-PTID       TO  DTCHK013-PTID
      *
           START   DTCHK013-FILE
               KEY IS  >=   DTCHK013-KEY
           INVALID
               MOVE    1               TO  FLG-DTCHK013
           NOT INVALID
               PERFORM 900-DTCHK013-READ-SEC
           END-START
      *
           .
      *
       900-DTCHK013-START-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（算定回数）チェックファイルREAD処理
      *****************************************************************
       900-DTCHK013-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK013
      *
           READ    DTCHK013-FILE       NEXT
           AT  END
               MOVE    1           TO  FLG-DTCHK013
           NOT AT END
               IF    ( DTCHK003-PTID   NOT =   DTCHK013-PTID )
                   MOVE    1       TO  FLG-DTCHK013
               END-IF
           END-READ
           IF    ( STS-COMMON  >   10 )
               MOVE    1           TO  FLG-DTCHK013
           END-IF
      *
           .
      *
       900-DTCHK013-READ-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタファイルSTART処理
      *****************************************************************
       900-DTCHK018-START-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      DTCHK018-REC
           MOVE    SPA-HOSPNUM         TO  DTCHK018-HOSPNUM
           MOVE    TCHK-CHK-CHKKBN     TO  DTCHK018-CHKKBN
           MOVE    TCHK-CHK-SRYCD      TO  DTCHK018-SRYCD
           START   DTCHK018-FILE
               KEY IS  >=   DTCHK018-KEY
           INVALID
               MOVE    1               TO  FLG-CHKMST
           NOT INVALID
               PERFORM 900-DTCHK018-READ-SEC
           END-START
      *
           .
      *
       900-DTCHK018-START-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタファイルREAD処理
      *****************************************************************
       900-DTCHK018-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-CHKMST
           MOVE    ZERO            TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                    OR   ( FLG-LOOP-END NOT =   ZERO )
      *
               READ    DTCHK018-FILE       NEXT
               AT  END
                   MOVE    1           TO  FLG-CHKMST
               NOT AT END
      *
                   IF    ( SPA-HOSPNUM     NOT =   DTCHK018-HOSPNUM )
                    OR   ( TCHK-CHK-CHKKBN NOT =   DTCHK018-CHKKBN )
                    OR   ( TCHK-CHK-SRYCD  NOT =   DTCHK018-SRYCD )
                       MOVE    1       TO  FLG-CHKMST
                   ELSE
                       MOVE    1       TO  FLG-LOOP-END
      *
                       IF    ( TCHK-CHK-CDKBN  NOT =   DTCHK018-CDKBN )
                           MOVE    ZERO    TO  FLG-LOOP-END
                       END-IF
      *
                       IF    ( DTCHK018-CHKERR =   8 )
                           MOVE    ZERO    TO  FLG-LOOP-END
                       END-IF
      *
                   END-IF
      *
               END-READ
               IF    ( STS-COMMON  >   10 )
                   MOVE    1           TO  FLG-CHKMST
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-DTCHK018-READ-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタファイルSTART処理
      *****************************************************************
       900-DTCHK022-START-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-DTCHK022
      *
           INITIALIZE                      DTCHK022-REC
           MOVE    KEY-SRYCD           TO  DTCHK022-ISAM-SRYCD
           START   DTCHK022-FILE
               KEY IS  >=   DTCHK022-ISAM-KEY
           INVALID
               MOVE    1               TO  FLG-DTCHK022
           NOT INVALID
               PERFORM 900-DTCHK022-READ-SEC
           END-START
      *
           .
      *
       900-DTCHK022-START-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタファイルREAD処理
      *****************************************************************
       900-DTCHK022-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK022
           MOVE    ZERO            TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-DTCHK022    NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               READ    DTCHK022-FILE       NEXT
               AT  END
                   MOVE    1       TO  FLG-DTCHK022
               NOT AT END
      *
                   IF    ( KEY-SRYCD   NOT =   DTCHK022-ISAM-SRYCD )
                       MOVE    1   TO  FLG-DTCHK022
                   ELSE
      *
                       MOVE   1        TO  FLG-LOOP-END
      *
                       IF    ( DTCHK022-ISAM-ITEMNO
                                       =   ZERO  )
                           MOVE    ZERO    TO  FLG-LOOP-END
                       END-IF
      *
                   END-IF
               END-READ
               IF    ( STS-COMMON  >   10 )
                   MOVE    1       TO  FLG-DTCHK022
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-DTCHK022-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者病名読込処理
      *****************************************************************
       900-PTBYOMEI-SELECT-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-PTBYOMEI
           INITIALIZE                      PTBYOMEI-REC
      *
           MOVE    DTCHK003-HOSPNUM     TO  PTBYO-HOSPNUM
           MOVE    DTCHK003-PTID       TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE "tbl_ptbyomei"     TO  MCP-TABLE
               MOVE "key6"             TO  MCP-PATHNAME
               PERFORM 900-PTBYOMEI-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYOMEI-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    患者病名読込処理
      *****************************************************************
       900-PTBYOMEI-FETCH-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
      *
                   MOVE    MCPDATA-REC TO  PTBYOMEI-REC
      *
                   MOVE    1           TO  FLG-LOOP-END
      *
               ELSE
                   MOVE    1           TO  FLG-PTBYOMEI
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-PTBYOMEI-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
      *
           MOVE    WRK-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    WRK-TNS-YMD         TO  TTNS-CHK-YMD
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                      TNS-TENSU-REC
               MOVE    TTNS-CHK-SRYCD      TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD        TO  TNS-YUKOSTYMD
                                               TNS-YUKOEDYMD
               MOVE    WRK-PARA-HOSPNUM    TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
      *
                   IF    ( TNS-SRYCD(1:1)  =   "6" OR "7" )
                       INITIALIZE              TNSUP-KEY
                       MOVE    WRK-KJNYMD  TO  TNSUP-YUKOSTYMD
                                               TNSUP-YUKOEDYMD
                       CALL    "ORCSSETTANI"   USING
                                               TNS-TENSU-REC
                                               SPA-AREA
                   END-IF
      *
      *            点数マスタ退避ワークに編集する
                   PERFORM 9001-TTNS-HEN-SEC
                ELSE
      *
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
      *
                   INITIALIZE                      ORCXKANACONVAREA
                   MOVE 300                    TO  KANACONV-LEN
                   MOVE 1000                   TO  KANACONV-STLEN
                   MOVE 1                      TO  KANACONV-CONV-FLAG
                   MOVE 15                     TO  KANACONV-CHAR-TYPE
                   MOVE WRK-SRYCD              TO  KANACONV-INDATA
                   CALL "kanaconv"         USING
                                           ORCXKANACONVAREA
                   MOVE    KANACONV-OUTDATA    TO  TNS-NAME
      *
               END-IF
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           .
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
      *    点数付加２検索
      *****************************************************************
       900-TENSUPLUS2-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSUPLUS2
      *
      *    点数付加２退避ワークチェック
           PERFORM 9001-TTNSP2-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNSP2-FLG          =   ZERO )
               INITIALIZE                      TNSP2-REC
               MOVE    WRK-SRYCD           TO  TNSP2-SRYCD
               MOVE    WRK-PARA-HOSPNUM    TO  TNSP2-HOSPNUM
               MOVE    TNSP2-REC           TO  MCPDATA-REC
               MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  TNSP2-REC
                   PERFORM 9001-TTNSP2-HEN-SEC
                ELSE
                   INITIALIZE              TNSP2-REC
                   MOVE    WRK-SRYCD       TO  TNSP2-SRYCD
                   MOVE    WRK-PARA-HOSPNUM TO TNSP2-HOSPNUM
                   PERFORM 9001-TTNSP2-HEN-SEC
               END-IF
      *
               MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           .
       900-TENSUPLUS2-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数付加２退避ワークチェック処理
      *****************************************************************
       9001-TTNSP2-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  TTNSP2-FLG
           INITIALIZE                      TNSP2-REC
      *
           PERFORM VARYING TTNSP2-IDX  FROM    1   BY  1
                   UNTIL ( TTNSP2-IDX  >   TTNSP2-MAX )
                    OR   ( TTNSP2-FLG  =   1 )
               IF    ( TTNSP2-SRYCD (TTNSP2-IDX)
                                       =   WRK-SRYCD )
                   MOVE    1           TO  TTNSP2-FLG
                   MOVE    TTNSP2-OCC (TTNSP2-IDX)
                                       TO  TNSP2-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNSP2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数付加２退避ワーク編集処理
      *****************************************************************
       9001-TTNSP2-HEN-SEC             SECTION.
      *
           IF    ( CONST-TTNSP2-MAX    >   TTNSP2-MAX )
               COMPUTE TTNSP2-MAX      =   TTNSP2-MAX    +   1
               MOVE    TNSP2-REC       TO  TTNSP2-OCC (TTNSP2-MAX)
           END-IF
      *
           .
       9001-TTNSP2-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者番号取得処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
           INITIALIZE                      PTNUM-REC
           MOVE    DTCHK003-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    DTCHK003-PTID       TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
           INITIALIZE                      PTINF-REC
           MOVE    DTCHK003-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    DTCHK003-PTID       TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  PTINF-REC
               ELSE
                   MOVE    1               TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
      *
               MOVE    PTINF-REC           TO  ERRPTINF-REC
      *
               MOVE    ERRPTINF-KANANAME   TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               MOVE    KANACONV-OUTDATA    TO  ERRPTINF-KANANAME
      *
               MOVE    ERRPTINF-NAME       TO  WRK-KANACHK-MAE-INPUT
               PERFORM 801-KANA-CHK-SEC
               MOVE    WRK-KANACHK-MAE-INPUT
                                           TO  ERRPTINF-NAME
      *
           ELSE
               MOVE    "患者情報の検索に失敗しました"
                                       TO  WRK-DTCHKERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴検索処理(KEY11)
      *****************************************************************
       900-SANTEI-KEY11-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SANTEI
      *
           INITIALIZE                      SANTEI-REC
           MOVE    DTCHK003-HOSPNUM     TO  SANTEI-HOSPNUM
           MOVE    DTCHK003-PTID       TO  SANTEI-PTID
           MOVE    ORCBSD01-SRYYM      TO  SANTEI-SRYYM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SANTEI-REC
           ELSE
               INITIALIZE                  SANTEI-REC
               MOVE    1               TO  FLG-SANTEI
           END-IF
      *
           .
      *
       900-SANTEI-KEY11-SEL-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴読込処理(KEY11)
      *****************************************************************
       900-SANTEI-KEY11-FET-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SANTEI-REC
           ELSE
               INITIALIZE                  SANTEI-REC
               MOVE    1               TO  FLG-SANTEI
           END-IF
      *
           .
      *
       900-SANTEI-KEY11-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報検索処理
      *****************************************************************
       900-PTHKNINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTHKNINF
           INITIALIZE                      PTHKNINF-REC
           MOVE    DTCHK003-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    DTCHK003-PTID       TO  PTHKN-PTID
           MOVE    WRK-HKNID           TO  PTHKN-HKNID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTHKNINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者労災保険情報検索処理
      *****************************************************************
       900-PTRSIINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTRSIINF
     *
           INITIALIZE                      PTRSIINF-REC
           MOVE    DTCHK003-HOSPNUM    TO  PTRSI-HOSPNUM
           MOVE    DTCHK003-PTID       TO  PTRSI-PTID
           MOVE    WRK-HKNID           TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
     *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTRSIINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者労災保険情報検索処理
      *****************************************************************
       900-PTRSIINF-KEY4-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-PTRSIINF
     *
           MOVE    "1"                 TO  PTRSI-HKNKBN
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
     *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTRSIINF-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    労災基準監督署コード検索処理
      *****************************************************************
       900-LABOR-SIO-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-LABOR-SIO
      *
           INITIALIZE                      LABOR-SIO-REC
           MOVE    PTRSI-SYOCD         TO  LABOR-SIO-SYOCD
           MOVE    WRK-KJNYMD          TO  LABOR-SIO-YUKOSTYMD
                                           LABOR-SIO-YUKOEDYMD
           MOVE    LABOR-SIO-REC       TO  MCPDATA-REC
           MOVE    "tbl_labor_sio"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  LABOR-SIO-REC
           ELSE
               MOVE    1               TO  FLG-LABOR-SIO
           END-IF
     *
           MOVE    "tbl_labor_sio" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-LABOR-SIO-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報検索処理
      *****************************************************************
       900-PTKOHINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTKOHINF
           INITIALIZE                      PTKOHINF-REC
           MOVE    DTCHK003-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    DTCHK003-PTID       TO  PTKOH-PTID
           MOVE    WRK-KOHID           TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTKOHINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病名検索処理
      *****************************************************************
       900-BYOMEI-KEY-SEL-SEC          SECTION.
      *
           INITIALIZE                      BYOMEI-REC
           MOVE    WRK-BYOMEICD        TO  BYO-BYOMEICD
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  BYOMEI-REC
           ELSE
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
       900-BYOMEI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC    SECTION.
      *
           MOVE    ZERO                    TO  FLG-NYUINACCT
      *
           INITIALIZE                          NYUINACCT-REC
           MOVE    DTCHK003-HOSPNUM        TO  NACCT-HOSPNUM
           MOVE    DTCHK003-PTID           TO  NACCT-PTID
           MOVE    ORCBSD01-SRYYM          TO  NACCT-SRYYM
           MOVE    WRK-ZAISKBKBN           TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC           TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC                  =   ZERO )
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               INITIALIZE                      NYUINACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理
      *****************************************************************
       900-NYUINACCT-KEY17-FET-SEC    SECTION.
      *
           MOVE    ZERO                    TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC                  =   ZERO )
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               INITIALIZE                      NYUINACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-KEY17-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェック例外マスタ検索処理
      *****************************************************************
       900-CHKREIGAI-KEY2-SEL-SEC      SECTION.
      *
           MOVE    ZERO                    TO  FLG-CHKREIGAI
      *
           INITIALIZE                          CHKREIGAI-REC
           MOVE    SPA-HOSPNUM             TO  CHKREIGAI-HOSPNUM
           MOVE    ORCBSD01-NYUGAIKBN      TO  CHKREIGAI-NYUGAIKBN
           MOVE    ORCBSD01-SRYYM          TO  CHKREIGAI-STYUKYMD
           MOVE    "01"                    TO  CHKREIGAI-STYUKYMD(7:2)
           MOVE    CHKREIGAI-STYUKYMD      TO  CHKREIGAI-EDYUKYMD
           MOVE    CHKREIGAI-REC           TO  MCPDATA-REC
           MOVE    "tbl_chkreigai"         TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHKREIGAI-REC
           ELSE
               INITIALIZE              CHKREIGAI-REC
               MOVE    1           TO  FLG-CHKREIGAI
           END-IF
      *
           .
       900-CHKREIGAI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェック例外マスタFETCH処理
      *****************************************************************
       900-CHKREIGAI-KEY2-FET-SEC      SECTION.
      *
           MOVE    "tbl_chkreigai"         TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHKREIGAI-REC
           ELSE
               INITIALIZE              CHKREIGAI-REC
               MOVE    1           TO  FLG-CHKREIGAI
           END-IF
      *
           .
       900-CHKREIGAI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    入力コード検索処理
      *****************************************************************
       900-INPUTCD-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-INPUTCD
      *
           INITIALIZE                  INPUTCD-REC
           MOVE    SPACE           TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM     TO  ICD-HOSPNUM
           MOVE    "6"             TO  ICD-CDSYU
           MOVE    WRK-SRYCD       TO  ICD-INPUTCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  INPUTCD-REC
               MOVE    ICD-DSPNAME     TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT   TO  ICD-DSPNAME
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1               TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-INPUTCD-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力セット検索処理
      *****************************************************************
       900-INPUTSET-KEY7-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-INPUTSET
      *
           INITIALIZE                  INPUTSET-REC
           MOVE    SPA-HOSPNUM     TO  ISET-HOSPNUM
           MOVE    WRK-SRYCD       TO  ISET-SETCD
           MOVE    WRK-KJNYMD      TO  ISET-YUKOSTYMD
                                       ISET-YUKOEDYMD
           MOVE    "31"            TO  ISET-YUKOSTYMD(7:2)
           MOVE    "01"            TO  ISET-YUKOEDYMD(7:2)
           MOVE    INPUTSET-REC    TO  MCPDATA-REC
           MOVE    "tbl_inputset"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  INPUTSET-REC
           ELSE
               INITIALIZE              INPUTSET-REC
               MOVE    1           TO  FLG-INPUTSET
           END-IF
      *
           .
       900-INPUTSET-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力セットFETCH処理
      *****************************************************************
       900-INPUTSET-KEY7-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-INPUTSET
      *
           MOVE    "tbl_inputset"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  INPUTSET-REC
           ELSE
               INITIALIZE              INPUTSET-REC
               MOVE    1           TO  FLG-INPUTSET
           END-IF
      *
           .
       900-INPUTSET-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    入力コード検索処理
      *****************************************************************
       900-SRYCDCHG-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYCDCHG
      *
           INITIALIZE                  SRYCDCHG-REC
           MOVE    SPACE           TO  SRYCDCHG-REC
           MOVE    SPA-HOSPNUM     TO  CHG-HOSPNUM
           MOVE    WRK-SRYCD       TO  CHG-IPNSRYCD
      *
           MOVE    SRYCDCHG-REC    TO  MCPDATA-REC
           MOVE    "tbl_srycdchg"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYCDCHG-REC
           ELSE
               INITIALIZE              SRYCDCHG-REC
               MOVE    1           TO  FLG-SRYCDCHG
           END-IF
      *
           MOVE    "tbl_srycdchg"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SRYCDCHG-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       900-HKNNUM-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
      *
           INITIALIZE                  HKNNUM-REC
      *
           PERFORM 9001-HKNNUM-KEY-SEL-SEC
           IF    ( THKN-HIT            =   ZERO )
      *
               INITIALIZE                      HKNNUM-REC
               MOVE    WRK-PARA-HOSPNUM    TO  HKN-HOSPNUM
               MOVE    WRK-KJNYMD          TO  HKN-TEKSTYMD
               MOVE    WRK-KJNYMD          TO  HKN-TEKEDYMD
               MOVE    THKN-SRH-HKNNUM     TO  HKN-HKNNUM
               MOVE    THKN-SRH-PAYKBN     TO  HKN-PAYKBN
               MOVE    HKNNUM-REC          TO  MCPDATA-REC
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF ( MCP-RC                 =   ZERO )
                   MOVE    MCPDATA-REC     TO  HKNNUM-REC
                   IF    ( THKN-MAX        >=  CONST-THKN-MAX )
                       CONTINUE
                   ELSE
                       COMPUTE THKN-MAX    =   THKN-MAX    +   1
                       MOVE    HKNNUM-REC  TO  THKN-OCC (THKN-MAX)
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
      *
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       900-HKNNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       9001-HKNNUM-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  THKN-HIT
      *
           PERFORM VARYING THKN-IDX    FROM    1   BY  1
                   UNTIL ( THKN-IDX    >   THKN-MAX )
                    OR   ( THKN-HIT    =   1 )
      *
               IF    ( WRK-PARA-HOSPNUM    =   THKN-HOSPNUM  (THKN-IDX))
                AND  ( THKN-SRH-HKNNUM     =   THKN-HKNNUM   (THKN-IDX))
                AND  ( THKN-SRH-PAYKBN     =   THKN-PAYKBN   (THKN-IDX))
                AND  ( WRK-KJNYMD          >=  THKN-TEKSTYMD (THKN-IDX))
                AND  ( WRK-KJNYMD          <=  THKN-TEKEDYMD (THKN-IDX))
                   MOVE    THKN-OCC (THKN-IDX)
                                       TO  HKNNUM-REC
                   MOVE    1           TO  THKN-HIT
               END-IF
      *
           END-PERFORM
      *
           .
       9001-HKNNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    月代わり受給者番号検索処理
      *****************************************************************
       900-MONTHLYNUM-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-MONTHLYNUM
      *
           INITIALIZE                  MONTHLYNUM-REC
           MOVE    PTKOH-HOSPNUM   TO  MONTHLYNUM-HOSPNUM
           MOVE    PTKOH-PTID      TO  MONTHLYNUM-PTID
           MOVE    PTKOH-KOHID     TO  MONTHLYNUM-KOHID
           MOVE    ORCBSD01-SRYYM  TO  MONTHLYNUM-SRYYM
           MOVE    ORCBSD01-NYUGAIKBN
                                   TO  MONTHLYNUM-NYUGAIKBN
           MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
           MOVE    "tbl_monthlynum" TO MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  MONTHLYNUM-REC
           ELSE
               INITIALIZE              MONTHLYNUM-REC
               MOVE    1           TO  FLG-MONTHLYNUM
           END-IF
      *
           MOVE    "tbl_monthlynum" TO MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-MONTHLYNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者コメント検索処理
      *****************************************************************
       900-PTCOM-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-PTCOM
      *
           INITIALIZE                      PTCOM-REC
           MOVE    SPA-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    DCSRY-PTID          TO  PTCOM-PTID
           MOVE    DCSRY-ZAINUM        TO  PTCOM-ZAINUM
           MOVE    DCSRY-SRYCD         TO  PTCOM-SRYCD
           MOVE    DCSRY-INPUTNUM      TO  PTCOM-RENNUM
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTCOM-REC
           ELSE
               MOVE    1               TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTCOM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY27-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    WRK-CHK-CHKKBN      TO  CHKSND-CHKKBN
           MOVE    WRK-CHK-SRYCD       TO  CHKSND-SRYCD
           MOVE    WRK-KJNYMD          TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    WRK-CHK-CDKBN       TO  CHKSND-CDKBN
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKSND-KEY27-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY28-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key28"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY28-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２FETCH処理
      *****************************************************************
       900-CHKSND-KEY28-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key28"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY28-FET-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ情報検索(KEY2)
      *****************************************************************
       900-TEKIOUBYOMEI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TEKBYO
      *
           INITIALIZE                  TEKIOUBYOMEI-REC
           MOVE    WRK-CHK-CHKKBN      TO  TEKBYO-CHKKBN
           MOVE    WRK-CHK-SRYCD       TO  TEKBYO-SRYCD
           MOVE    TEKIOUBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "tbl_tekioubyomei"  TO   MCP-TABLE
           MOVE    "key2"              TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEKIOUBYOMEI-REC
           ELSE
               INITIALIZE              TEKIOUBYOMEI-REC
               MOVE    1               TO  FLG-TEKBYO
           END-IF
           .
       900-TEKIOUBYOMEI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ情報FETCH(KEY2)
      *****************************************************************
       900-TEKIOUBYOMEI-KEY2-FET-SEC       SECTION.
      *
           MOVE    "tbl_tekioubyomei"  TO   MCP-TABLE
           MOVE    "key2"              TO   MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEKIOUBYOMEI-REC
           ELSE
               INITIALIZE              TEKIOUBYOMEI-REC
               MOVE    1               TO  FLG-TEKBYO
           END-IF
           .
      *
       900-TEKIOUBYOMEI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    医薬品傷病名マスタ検索
      *****************************************************************
       900-TEKIOU-DISEASE-KEY3-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-TEKDIS
      *
           INITIALIZE                      TEKIOU-DISEASE-REC
           MOVE    WRK-CHK-SRYCD       TO  TEKDIS-SELSRYCD
           MOVE    TEKIOU-DISEASE-REC  TO  MCPDATA-REC
           MOVE    "tbl_tekiou_disease" TO  MCP-TABLE
           MOVE    "key3"              TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEKIOU-DISEASE-REC
           ELSE
               INITIALIZE                  TEKIOU-DISEASE-REC
               MOVE    1               TO  FLG-TEKDIS
           END-IF
           .
       900-TEKIOU-DISEASE-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    医薬品傷病名マスタFETCH
      *****************************************************************
       900-TEKIOU-DISEASE-KEY3-FET-SEC       SECTION.
      *
           MOVE    "tbl_tekiou_disease" TO  MCP-TABLE
           MOVE    "key3"              TO   MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEKIOU-DISEASE-REC
           ELSE
               INITIALIZE                  TEKIOU-DISEASE-REC
               MOVE    1               TO  FLG-TEKDIS
           END-IF
           .
      *
       900-TEKIOU-DISEASE-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者介護認定者情報検索処理
      *****************************************************************
       900-PTCARE-NINTEI-KEY1-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTCRNIN
      *
           INITIALIZE                  PTCRNIN-REC
      *
           MOVE    SPA-HOSPNUM         TO   PTCRNIN-HOSPNUM
           MOVE    DTCHK003-PTID       TO   PTCRNIN-PTID
           MOVE    WRK-GETUMATU        TO   PTCRNIN-TEKSTYMD
           MOVE    WRK-KJNYMD          TO   PTCRNIN-TEKEDYMD
           MOVE    PTCRNIN-REC         TO   MCPDATA-REC
           MOVE    "tbl_ptcare_nintei" TO   MCP-TABLE
           MOVE    "key1"              TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTCRNIN-REC
           ELSE
               INITIALIZE                  PTCRNIN-REC
               MOVE    1               TO  FLG-PTCRNIN
           END-IF
           .
      *
       900-PTCARE-NINTEI-KEY1-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者介護認定者情報FETCH処理
      *****************************************************************
       900-PTCARE-NINTEI-KEY1-FET-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTCRNIN
      *
           MOVE    "tbl_ptcare_nintei" TO   MCP-TABLE
           MOVE    "key1"              TO   MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTCRNIN-REC
           ELSE
               INITIALIZE                  PTCRNIN-REC
               MOVE    1               TO  FLG-PTCRNIN
           END-IF
           .
      *
       900-PTCARE-NINTEI-KEY1-FET-EXT.
           EXIT.
      *****************************************************************
      *    特定器材関連手技マスタ検索処理
      *****************************************************************
       900-MATERIAL-K-KEY2-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-MATERK
      *
           INITIALIZE                      MATERIAL-K-REC
      *
           MOVE    TCHK-CHK-SRYCD      TO  MATERK-SRYCD
           MOVE    MATERIAL-K-REC      TO  MCPDATA-REC
           MOVE    "tbl_material_k"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MATERIAL-K-REC
           ELSE
               INITIALIZE                  MATERIAL-K-REC
               MOVE    1               TO  FLG-MATERK
           END-IF
      *
           .
      *
       900-MATERIAL-K-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    特定器材関連手技マスタFETCH処理
      *****************************************************************
       900-MATERIAL-K-KEY2-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-MATERK
      *
           MOVE    "tbl_material_k"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MATERIAL-K-REC
           ELSE
               INITIALIZE                  MATERIAL-K-REC
               MOVE    1               TO  FLG-MATERK
           END-IF
      *
           .
      *
       900-MATERIAL-K-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHを行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＣＲＥＡＴＥ処理
      *****************************************************************
       910-DBCREATE-SEC                SECTION.
      *
           MOVE    "DBCREATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCREATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＣＲＥＡＴＥＩＮＤＥＸ処理
      *****************************************************************
       910-DBCREATEINDEX-SEC           SECTION.
      *
           MOVE    "DBCREATEINDEX"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCREATEINDEX-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＲＯＰ処理
      *****************************************************************
       910-DBDROP-SEC                  SECTION.
      *
           MOVE    "DBDROP"            TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDROP-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                 SECTION.
      *
           IF    ( LNK-PRTKANRI-TBL-KEY    NOT =  "RECEPTX" )
               MOVE    LOW-VALUE           TO  MCP-TABLE
               MOVE    LOW-VALUE           TO  MCP-PATHNAME
               MOVE    "DBSTART"           TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-IF
           .
       900-DBSTART-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC                SECTION.
      *
           IF    ( LNK-PRTKANRI-TBL-KEY    NOT =  "RECEPTX" )
               MOVE    LOW-VALUE           TO  MCP-TABLE
               MOVE    LOW-VALUE           TO  MCP-PATHNAME
               MOVE    "DBCOMMIT"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-IF
           .
       900-DBCOMMIT-EXT.
           EXIT.
