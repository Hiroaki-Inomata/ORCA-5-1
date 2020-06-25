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
       PROGRAM-ID.                     ORCDTCHK011.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 縦覧データ取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  13/12/10    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
      *    データチェック中間ファイル（対象患者）
           SELECT  DTCHK003-FILE   ASSIGN  DTCHK003PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
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
      *    データチェック中間ファイル（電子点数表−算定回数）
           SELECT  DTCHK014-FILE
                                   ASSIGN  DTCHK014PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK014-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（点数付加）
           SELECT  DTCHK015-FILE
                                   ASSIGN  DTCHK015PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK015-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（点数）
           SELECT  DTCHK016-FILE
                                   ASSIGN  DTCHK016PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK016-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    保険組合せ
           SELECT  DTCHK017-FILE   ASSIGN  DTCHK017PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK017-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    チェックマスタ
           SELECT  DTCHK018-FILE   ASSIGN  DTCHK018PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK018-KEY
                                   FILE    STATUS  IS  STS-COMMON.
      *
      *    データチェック中間ファイル（診療行為コード）
           SELECT  DTCHK019-FILE   ASSIGN  DTCHK019PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  DTCHK019-KEY
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
      *    データチェック中間ファイル（対象患者情報）
       FD  DTCHK003-FILE.
           COPY    "CPDTCHK001.INC"
                                   REPLACING  //DTCHK001//
                                   BY         //DTCHK003//.
      *
      *    データチェック中間ファイル（算定回数）
       FD  DTCHK013-FILE.
           COPY    "CPDTCHK013.INC".
      *
      *    データチェック中間ファイル（電子点数表−算定回数）
       FD  DTCHK014-FILE.
       01  DTCHK014-REC.
           COPY    "CPETENSU5.INC" REPLACING  //ETENSU5//
                                   BY         //DTCHK014//.
      *
      *    データチェック中間ファイル（点数付加）
       FD  DTCHK015-FILE.
       01  DTCHK015-REC.
           COPY    "CPTENSUPLUS.INC"
                                   REPLACING  //TNSP//
                                   BY         //DTCHK015//.
      *
      *    データチェック中間ファイル（点数）
       FD  DTCHK016-FILE.
           COPY    "CPTENSU.INC"   REPLACING  //TNS//
                                   BY         //DTCHK016//.
      *
      *    データチェック中間ファイル（保険組合せ）
       FD  DTCHK017-FILE.
       01  DTCHK017-REC.
           COPY    "CPHKNCOMBI.INC" REPLACING  //COMB-//
                                    BY         //DTCHK017-//.
      *
      *    チェックマスタ
       FD  DTCHK018-FILE.
       01  DTCHK018-REC.
           COPY    "CPCHK.INC"      REPLACING  //CHK-//
                                    BY         //DTCHK018-//.
      *
      *    データチェック中間ファイル（診療行為コード）
       FD  DTCHK019-FILE.
       01  DTCHK019-REC.
           03  DTCHK019-KEY.
               05  DTCHK019-SRYCD      PIC X(09).
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
      *    中間ファイルファイル名称領域 
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHKACCTPARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHKSRYPARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK003PARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK013PARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK014PARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK015PARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK016PARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK017PARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK018PARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK019PARA//.
      *
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //DTCHK022PARA//.
      *
           COPY    "CPASGNPARA.INC".
      *
      *    エラーファイルファイル名称領域 
           COPY    "CPRECEERR.INC"   REPLACING  //RECEERR//
                                   BY         //DTCHKERRPARA//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-COMMON                              PIC X(02).
           03  STS-DTCHKERR                            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ERR                                 PIC 9(01).
           03  FLG-DCACCT                              PIC 9(01).
           03  FLG-DCSRY                               PIC 9(01).
           03  FLG-DTCHK003                            PIC 9(01).
           03  FLG-DTCHK013                            PIC 9(01).
           03  FLG-DTCHK014                            PIC 9(01).
           03  FLG-DTCHK015                            PIC 9(01).
           03  FLG-DTCHK016                            PIC 9(01).
           03  FLG-DTCHK017                            PIC 9(01).
           03  FLG-DTCHK018                            PIC 9(01).
           03  FLG-DTCHK019                            PIC 9(01).
           03  FLG-DTCHK022                            PIC 9(01).
           03  FLG-LOOP-END                            PIC 9(01).
           03  FLG-PTNYUINRRK                          PIC 9(01).
           03  FLG-ETENSU1                             PIC 9(01).
           03  FLG-ETENSU2                             PIC 9(01).
           03  FLG-ETENSU3                             PIC 9(01).
           03  FLG-ETENSU5                             PIC 9(01).
           03  FLG-DOJITSU                             PIC 9(01).
           03  FLG-SRYACT                              PIC 9(01).
           03  FLG-SRYACCT                             PIC 9(01).
           03  FLG-NSRYSRH                             PIC 9(01).
           03  FLG-NRRKSRH                             PIC 9(01).
           03  FLG-TENSU                               PIC 9(01).
           03  FLG-TENSUPLUS                           PIC 9(01).
           03  FLG-SYSKANRI                            PIC 9(01).
           03  FLG-HKNCOMBI                            PIC 9(01).
           03  FLG-CHKMST                              PIC 9(01).
           03  FLG-RKISAI                              PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-ETENSU5                             PIC 9(05).
           03  CNT-TEKCD                               PIC 9(05).
      *
       01  IDX-AREA.
           03  IDX0                                    PIC 9(05).
           03  IDX1                                    PIC 9(05).
           03  IDX2                                    PIC 9(05).
           03  IDX3                                    PIC 9(05).
           03  IDX4                                    PIC 9(05).
           03  IDX5                                    PIC 9(05).
           03  IDX6                                    PIC 9(05).
           03  IDXA                                    PIC 9(05).
           03  IDXD                                    PIC 9(05).
           03  IDXK                                    PIC 9(05).
           03  IDXR                                    PIC 9(05).
           03  IDXS                                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-DTCHKERR                            PIC X(200).
           03  WRK-SYMD                                PIC X(08).
           03  WRK-ZOGENPTN                            PIC X(01).
           03  WRK-ZOGEN                               PIC S9(05).
           03  WRK-KEISANYMD.
               05  WRK-KEISANYM                        PIC X(06).
               05  WRK-KEISANDD                        PIC 9(02).
           03  WRK-MIN-SRYYM                           PIC X(06).
           03  WRK-NYUINYMD                            PIC X(08).
           03  WRK-BOMYMD                              PIC X(08).
           03  WRK-EOMYMD                              PIC X(08).
           03  WRK-LASTYM                              PIC X(06).
           03  WRK-STDAY                               PIC 9(02).
           03  WRK-EDDAY                               PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYM                           PIC X(06).
               05  WRK-SRYDD                           PIC 9(02).
           03  WRK-DOW                                 PIC 9(01).
           03  WRK-WEEK                                PIC 9(01).
           03  WRK-TW-SUNDAY                           PIC X(08).
           03  WRK-TW-SATDAY                           PIC X(08).
           03  WRK-LW-SUNDAY                           PIC X(08).
           03  WRK-NW-SUNDAY                           PIC X(08).
           03  WRK-KAISU                               PIC 9(05).
           03  WRK-RRK-MAX                             PIC 9(05).
           03  WRK-OUTYMD.
               05  WRK-OUTYM                           PIC X(06).
               05  WRK-OUTDD                           PIC 9(02).
           03  WRK-MSTKBN                              PIC X(01).
           03  WRK-JGNCNTERR                           PIC 9(1).
           03  WRK-TSUKISU                             PIC 9(02).
           03  WRK-TSUKISUZ1                           PIC Z.
           03  WRK-TSUKISUZ2                           PIC ZZ.
           03  WRK-KANACHK-STR                         PIC X(500).
           03  WRK-PTID                                PIC 9(10).
           03  WRK-HKNCOMBINUMX.
               05  WRK-HKNCOMBINUM                     PIC 9(04).
           03  WRK-TABLE                               PIC X(64).
           03  WRK-PATHNAME                            PIC X(64).
           03  WRK-NYUGAITEKKBN                        PIC 9(01).
      *
       01  KEY-AREA.
           03  KEY-PTID                                PIC 9(10).
      *
       01  SEL-AREA.
           03  SEL-SRYCD                               PIC X(09).
           03  SEL-CHK-AREA.
               05  SEL-CHKKBN                          PIC X(01).
               05  SEL-CDKBN                           PIC X(01).
               05  SEL-CD                              PIC X(09).
               05  SEL-DATAKBN                         PIC X(01).
           03  SEL-ETENSU-AREA.
               05  SEL-H-TANI                          PIC 9(02).
               05  SEL-H-GROUP                         PIC X(07).
      *
       01  TDOJITSU-AREA.
           03  TDOJITSU-MAX                            PIC 9(05).
           03  TDOJITSU-OCC                            OCCURS 50.
               05  TDOJITSU-NYUINYMD                   PIC X(08).
      *
       01  TNYUINRRK-AREA.
           03  TNYUINRRK-SHONUM-MAX                    PIC 9(05).
           03  TNYUINRRK-OCC1                          OCCURS 10.
               05  TNYUINRRK-SHONUM                    PIC 9(03).
               05  TNYUINRRK-NYUINYMD                  PIC X(08).
               05  TNYUINRRK-TAIINYMD                  PIC X(08).
               05  TNYUINRRK-RRK-MAX                   PIC 9(05).
               05  TNYUINRRK-OCC2                      OCCURS 50.
                   07  TNYUINRRK-RRKNUM                PIC 9(03).
                   07  TNYUINRRK-DOJITSU               PIC 9(01).
                   07  TNYUINRRK-SRYSTYMD              PIC X(08).
                   07  TNYUINRRK-SRYEDYMD              PIC X(08).
                   07  TNYUINRRK-JTIKBN                PIC X(01).
           03  TNYUINRRK-DAY-G.
               05  TNYUINRRK-DAY-OCC1                  OCCURS 2.
                   07  TNYUINRRK-DAY-OCC2              OCCURS 31.
                       09  TNYUINRRK-DAY-SHONUM        PIC 9(03).
                       09  TNYUINRRK-DAY-NYUINYMD      PIC X(08).
                       09  TNYUINRRK-DAY-NYUINYMD2     PIC X(08).
                       09  TNYUINRRK-DAY-TAIINYMD      PIC X(08).
                       09  TNYUINRRK-DAY-TAIINYMD2     PIC X(08).
                       09  TNYUINRRK-DAY-RRKNUM        PIC 9(03).
      *
           COPY    "COMMON-SPA".
      *
       01  CONST-AREA.
           03  CONST-TDOJITSU-MAX              PIC 9(03) VALUE 50.
           03  CONST-SHONUM-MAX                PIC 9(03) VALUE 10.
           03  CONST-RRK-MAX                   PIC 9(03) VALUE 50.
           03  CONST-HKNCOMBI-MAX              PIC 9(03) VALUE 40.
      *
       01  TNRRK-REC.
           COPY    "CPPTNYUINRRK.INC"
                                   REPLACING  //PTNYUINRRK//
                                   BY         //TNRRK//.
      *
           COPY    "CPDTCHK013.INC"
                                   REPLACING  //DTCHK013//
                                   BY         //TMP013//.
      *
       01  MASTER-AREA.
           COPY    "CPETENSU5.INC"
                                   REPLACING  //ETENSU5//
                                   BY         //MASTER//.
      *
       01  TNSCHK-AREA.
           COPY    "CPTENSUPLUS.INC"
                                   REPLACING  //TNSP//
                                   BY         //TNSCHK//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1101.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    電子点数表（補助）
       01  ETENSU1-REC.
           COPY    "CPETENSU1.INC".
      *
      *    電子点数表（包括）
       01  ETENSU2-REC.
           COPY    "CPETENSU2.INC".
      *
      *    電子点数表（背反）
       01  ETENSU3-REC.
           COPY    "CPETENSU3.INC".
      *
      *    電子点数表（算定回数）
       01  ETENSU5-REC.
           COPY    "CPETENSU5.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    入院診療行為照会
       01  NSRYSRH-REC.
           COPY    "CPNSRYSRH.INC".
      *
      *    入院履歴照会
       01  NRRKSRH-REC.
           COPY    "CPNRRKSRH.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ付加情報
       01  TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    チェックマスタ
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    レセプト記載マスタ
       01  RKISAI-REC.
           COPY    "CPRECEKISAI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角半角チェック
           COPY    "CPORCSKANACHK.INC".
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
                                       KEY-AREA
                                       SEL-AREA
                                       TNYUINRRK-AREA
                                       TDOJITSU-AREA
                                       SPA-AREA
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
               MOVE    "ORCDTCHK011"       TO  JOB-PGID
               MOVE    "縦覧データ取得"
                                           TO JOB-SHELLMSG
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
      *
           END-IF
      *
           MOVE    ZERO                TO  FLG-END
                                           FLG-ERR
      *
      *    データチェック中間ファイル名
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHKACCTPARA-HOSPNUM
           MOVE    "DCACCT00"          TO  DTCHKACCTPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHKACCTPARA-TERMID
           MOVE    DTCHKACCTPARA-BASENAME
                                       TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHKACCTPARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHKSRYPARA-HOSPNUM
           MOVE    "DCSRY000"          TO  DTCHKSRYPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHKSRYPARA-TERMID
           MOVE    DTCHKSRYPARA-BASENAME
                                       TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHKSRYPARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK003PARA-HOSPNUM
           MOVE    "DTCHK003"          TO  DTCHK003PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK003PARA-TERMID
           MOVE    DTCHK003PARA-BASENAME
                                       TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK003PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK013PARA-HOSPNUM
           MOVE    "DTCHK013"          TO  DTCHK013PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK013PARA-TERMID
           MOVE    DTCHK013PARA-BASENAME
                                       TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK013PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK014PARA-HOSPNUM
           MOVE    "DTCHK014"          TO  DTCHK014PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK014PARA-TERMID
           MOVE    DTCHK014PARA-BASENAME
                                       TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK014PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK015PARA-HOSPNUM
           MOVE    "DTCHK015"          TO  DTCHK015PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK015PARA-TERMID
           MOVE    DTCHK015PARA-BASENAME
                                       TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK015PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK016PARA-HOSPNUM
           MOVE    "DTCHK016"          TO  DTCHK016PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK016PARA-TERMID
           MOVE    DTCHK016PARA-BASENAME
                                       TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK016PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK017PARA-HOSPNUM
           MOVE    "DTCHK017"          TO  DTCHK017PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK017PARA-TERMID
           MOVE    DTCHK017PARA-BASENAME
                                       TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK017PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK018PARA-HOSPNUM
           MOVE    "DTCHK018"          TO  DTCHK018PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK018PARA-TERMID
           MOVE    DTCHK018PARA-BASENAME
                                       TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK018PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK019PARA-HOSPNUM
           MOVE    "DTCHK019"          TO  DTCHK019PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK019PARA-TERMID
           MOVE    DTCHK019PARA-BASENAME
                                       TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK019PARA-FULLNAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  DTCHK022PARA-HOSPNUM
           MOVE    "DTCHK022"          TO  DTCHK022PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  DTCHK022PARA-TERMID
           MOVE    DTCHK022PARA-BASENAME
                                       TO  ASGNPARA
           PERFORM 800-ORCSGETTEMP-SEC
           MOVE    ASGNPARA            TO  DTCHK022PARA-FULLNAME
      *
           OPEN    INPUT               DCACCT-FILE
      *
           OPEN    OUTPUT              DTCHK014-FILE
           CLOSE   DTCHK014-FILE
           OPEN    I-O                 DTCHK014-FILE
      *
           OPEN    OUTPUT              DTCHK015-FILE
           CLOSE   DTCHK015-FILE
           OPEN    I-O                 DTCHK015-FILE
      *
           OPEN    OUTPUT              DTCHK016-FILE
           CLOSE   DTCHK016-FILE
           OPEN    I-O                 DTCHK016-FILE
      *
           OPEN    OUTPUT              DTCHK017-FILE
           CLOSE   DTCHK017-FILE
           OPEN    I-O                 DTCHK017-FILE
      *
           OPEN    OUTPUT              DTCHK013-FILE
           CLOSE   DTCHK013-FILE
      *
           OPEN    OUTPUT              DTCHK018-FILE
           CLOSE   DTCHK018-FILE
      *
           OPEN    OUTPUT              DTCHK019-FILE
           CLOSE   DTCHK019-FILE
           OPEN    I-O                 DTCHK019-FILE
      *
           OPEN    OUTPUT              DTCHK022-FILE
           CLOSE   DTCHK022-FILE
      *
      *    初期値設定処理
           PERFORM 1001-INIT-VAL-SET-SEC
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1101"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1101-REC
      *
           OPEN    INPUT               DCSRY-FILE
      *
           PERFORM 902-DCSRY-READ-SEC
      *
           PERFORM UNTIL ( FLG-DCSRY   NOT =   ZERO )
      *
               MOVE    DCSRY-SRYCD TO  SEL-SRYCD
               PERFORM 900-DTCHK019-READ-SEC
               IF    ( FLG-DTCHK019    NOT =   ZERO )
                   INITIALIZE          DTCHK019-REC
                   MOVE    DCSRY-SRYCD TO  DTCHK019-SRYCD
                   WRITE   DTCHK019-REC
               END-IF
      *
               PERFORM 902-DCSRY-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   DCSRY-FILE
      *
           OPEN    INPUT               DCSRY-FILE
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期値設定処理
      *****************************************************************
       1001-INIT-VAL-SET-SEC           SECTION.
      *
           MOVE    ORCBSD01-SRYYM      TO  WRK-BOMYMD
           MOVE    "01"                TO  WRK-BOMYMD (7:2)
      *
           MOVE    WRK-BOMYMD          TO  WRK-SYMD
           MOVE    "2"                 TO  WRK-ZOGENPTN
           MOVE    -1                  TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-SYMD            TO  WRK-LASTYM
      *
           MOVE    WRK-BOMYMD          TO  SPA-SYSYMD
                                           SPA-SRYYMD
      *
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-GETUMATU-SEC
           MOVE    WRK-SYMD            TO  WRK-EOMYMD
      *
           .
       1001-INIT-VAL-SET-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( ORCBSD01-CHKKBN (20)    =   "2" )
      *
      *        算定回数編集処理
               PERFORM 2001-KAISU-EDIT-SEC
      *
      *        過去算定回数編集処理
               PERFORM 2001-KAKO-KAISU-EDIT-SEC
      *
           END-IF
      *
      *    参照マスタ編集処理
           PERFORM 2001-REFMST-EDIT-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    算定回数編集処理
      *****************************************************************
       2001-KAISU-EDIT-SEC             SECTION.
      *
           OPEN    INPUT               DTCHK003-FILE
      *
           OPEN    I-O                 DTCHK013-FILE
      *
           MOVE    ZERO            TO  KEY-PTID
      *
           PERFORM 900-DTCHK003-READ-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK003    NOT =   ZERO )
      *
               MOVE    DTCHK003-PTID   TO  KEY-PTID
      *
               IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
                   PERFORM 2100-TDOJITSU-EDIT-SEC
                   PERFORM 2100-TNYUINRRK-EDIT-SEC
               END-IF
      *
               PERFORM 900-DCACCT-START-SEC
      *
               PERFORM UNTIL ( FLG-DCACCT      NOT =   ZERO )
                        OR   ( FLG-END         NOT =   ZERO )
      *
                   PERFORM 900-DCSRY-START-SEC
      *
                   PERFORM UNTIL ( FLG-DCSRY   NOT =   ZERO )
                            OR   ( FLG-END     NOT =   ZERO )
      *
                       IF    ( SYS-1101-SRYKAISUMSTKBN =   "1" )
                           PERFORM 20011-KAISU-EDIT-SEC
                       END-IF
      *
                       PERFORM 20012-KAISU-EDIT-SEC
      *
                       PERFORM 900-DCSRY-READ-SEC
      *
                   END-PERFORM
      *
                   PERFORM 900-DCACCT-READ-SEC
      *
               END-PERFORM
      *
               PERFORM UNTIL ( FLG-DTCHK003    NOT =   ZERO )
                        OR   ( DTCHK003-PTID   NOT =   KEY-PTID )
      *
                   PERFORM 900-DTCHK003-READ-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           CLOSE   DTCHK013-FILE
                   DTCHK003-FILE
      *
           .
       2001-KAISU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    算定回数編集処理（電子点数表より）
      *****************************************************************
       20011-KAISU-EDIT-SEC            SECTION.
      *
           PERFORM 900-DTCHK014-START-SEC
       *
           PERFORM UNTIL ( FLG-DTCHK014    NOT =   ZERO )
      *
               MOVE    DTCHK014-REC    TO  ETENSU5-REC
      *
               PERFORM 200111-DTCHK013-EDIT-SEC
      *
               PERFORM 900-DTCHK014-READ-SEC
      *
           END-PERFORM
      *
           .
       20011-KAISU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者・単位別算定回数ファイル編集処理
      *****************************************************************
       200111-DTCHK013-EDIT-SEC        SECTION.
      *
           MOVE    ETENSU5-REC         TO  MASTER-AREA
           MOVE    "2"                 TO  WRK-MSTKBN
           IF    ( ETENSU5-TOKUREI     =   1 )
               MOVE    9               TO  WRK-JGNCNTERR
           ELSE
               MOVE    ZERO            TO  WRK-JGNCNTERR
           END-IF
           MOVE    ZERO                TO  WRK-TSUKISU
           MOVE    ZERO                TO  IDX4
      *
           EVALUATE    ETENSU5-TANICD
      *    患者当り
           WHEN    53
               INITIALIZE                  DTCHK013-REC
               MOVE    "00000000"     TO  DTCHK013-SRYSTYMD
               MOVE    WRK-EOMYMD     TO  DTCHK013-SRYEDYMD
               PERFORM 900-DTCHK013-READ-SEC
               PERFORM 2001111-DTCHK013-EDIT-SEC
      *
      *    日
           WHEN   121
               INITIALIZE                  DTCHK013-REC
               MOVE    DCACCT-SRYYMD   TO  DTCHK013-SRYSTYMD
                                           DTCHK013-SRYEDYMD
                                           DTCHK013-SRYYMD
               PERFORM 900-DTCHK013-READ-SEC
               PERFORM 2001111-DTCHK013-EDIT-SEC
      *    月
           WHEN   131
               IF    ( DCACCT-SRYKBN   =   "90" OR "92" )
                   PERFORM 2001111-NYUIN-EDIT-SEC
               ELSE
                   INITIALIZE                  DTCHK013-REC
                   MOVE    WRK-BOMYMD      TO  DTCHK013-SRYSTYMD
                   MOVE    WRK-EOMYMD      TO  DTCHK013-SRYEDYMD
                   PERFORM 900-DTCHK013-READ-SEC
                   MOVE    DCACCT-SRYYMD (1:6)
                                           TO  DTCHK013-SRYYM
                   PERFORM 2001111-DTCHK013-EDIT-SEC
               END-IF
      *    入院初日
           WHEN   132
               PERFORM 2001111-NYUIN-EDIT-SEC
      *    入院中
           WHEN   133
               PERFORM 2001111-NYUIN-EDIT-SEC
      *    退院時
           WHEN   134
               PERFORM 2001111-NYUIN-EDIT-SEC
      *    週
           WHEN   138
               INITIALIZE              DTCHK013-REC
               MOVE    DCACCT-SRYYMD   TO  WRK-SYMD
               PERFORM 800-DOW-SEC
               MOVE    WRK-TW-SUNDAY   TO  DTCHK013-SRYSTYMD
               MOVE    WRK-TW-SATDAY   TO  DTCHK013-SRYEDYMD
               PERFORM 900-DTCHK013-READ-SEC
               MOVE    WRK-WEEK        TO  DTCHK013-WEEK
               PERFORM 2001111-DTCHK013-EDIT-SEC
      *    ２週
           WHEN   142
               INITIALIZE              DTCHK013-REC
               MOVE    DCACCT-SRYYMD   TO  WRK-SYMD
               PERFORM 800-DOW-SEC
               MOVE    WRK-LW-SUNDAY   TO  DTCHK013-SRYSTYMD
               MOVE    WRK-TW-SATDAY   TO  DTCHK013-SRYEDYMD
               PERFORM 900-DTCHK013-READ-SEC
               MOVE    WRK-WEEK        TO  DTCHK013-WEEK
               PERFORM 2001111-DTCHK013-EDIT-SEC
               IF    ( DCACCT-SRYYMD(1:6)  =   WRK-NW-SUNDAY(1:6))
                   INITIALIZE                      DTCHK013-REC
                   COMPUTE DTCHK013-WEEK = WRK-WEEK + 1
                   PERFORM 900-DTCHK013-READ-SEC
                   MOVE    WRK-TW-SUNDAY       TO  DTCHK013-SRYSTYMD
                   PERFORM 2001112-DTCHK013-EDIT-SEC
               END-IF
      *    ２月
           WHEN   143
               INITIALIZE              DTCHK013-REC
               MOVE    WRK-BOMYMD      TO  WRK-SYMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               MOVE    -1              TO  WRK-ZOGEN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-SYMD        TO  DTCHK013-SRYSTYMD
               MOVE    WRK-EOMYMD     TO  DTCHK013-SRYEDYMD
               PERFORM 900-DTCHK013-READ-SEC
               MOVE    DCACCT-SRYYMD (1:6)
                                       TO  DTCHK013-SRYYM
               PERFORM 2001111-DTCHK013-EDIT-SEC
      *    ３月
           WHEN   144
               INITIALIZE              DTCHK013-REC
               MOVE    WRK-BOMYMD      TO  WRK-SYMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               MOVE    -2              TO  WRK-ZOGEN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-SYMD        TO  DTCHK013-SRYSTYMD
               MOVE    WRK-EOMYMD     TO  DTCHK013-SRYEDYMD
               PERFORM 900-DTCHK013-READ-SEC
               MOVE    DCACCT-SRYYMD (1:6)
                                       TO  DTCHK013-SRYYM
               PERFORM 2001111-DTCHK013-EDIT-SEC
      *    ４月
           WHEN   145
               INITIALIZE              DTCHK013-REC
               MOVE    WRK-BOMYMD      TO  WRK-SYMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               MOVE    -3              TO  WRK-ZOGEN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-SYMD        TO  DTCHK013-SRYSTYMD
               MOVE    WRK-EOMYMD     TO  DTCHK013-SRYEDYMD
               PERFORM 900-DTCHK013-READ-SEC
               MOVE    DCACCT-SRYYMD (1:6)
                                       TO  DTCHK013-SRYYM
               PERFORM 2001111-DTCHK013-EDIT-SEC
      *    ６月
           WHEN   146
               INITIALIZE              DTCHK013-REC
               MOVE    WRK-BOMYMD      TO  WRK-SYMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               MOVE    -5              TO  WRK-ZOGEN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-SYMD        TO  DTCHK013-SRYSTYMD
               MOVE    WRK-EOMYMD     TO  DTCHK013-SRYEDYMD
               PERFORM 900-DTCHK013-READ-SEC
               MOVE    DCACCT-SRYYMD (1:6)
                                       TO  DTCHK013-SRYYM
               PERFORM 2001111-DTCHK013-EDIT-SEC
      *    １２月
           WHEN   147
               INITIALIZE              DTCHK013-REC
               MOVE    WRK-BOMYMD      TO  WRK-SYMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               MOVE    -11             TO  WRK-ZOGEN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-SYMD        TO  DTCHK013-SRYSTYMD
               MOVE    WRK-EOMYMD     TO  DTCHK013-SRYEDYMD
               PERFORM 900-DTCHK013-READ-SEC
               MOVE    DCACCT-SRYYMD (1:6)
                                       TO  DTCHK013-SRYYM
               PERFORM 2001111-DTCHK013-EDIT-SEC
      *    ５年
           WHEN   148
               INITIALIZE              DTCHK013-REC
               MOVE    WRK-BOMYMD      TO  WRK-SYMD
               MOVE    "2"             TO  WRK-ZOGENPTN
               COMPUTE WRK-ZOGEN   =   12 * -5 + 1
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-SYMD        TO  DTCHK013-SRYSTYMD
               MOVE    WRK-EOMYMD     TO  DTCHK013-SRYEDYMD
               PERFORM 900-DTCHK013-READ-SEC
               MOVE    DCACCT-SRYYMD (1:6)
                                       TO  DTCHK013-SRYYM
               PERFORM 2001111-DTCHK013-EDIT-SEC
           END-EVALUATE
      *
           .
       200111-DTCHK013-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者・単位別算定回数ファイル編集処理（入院）
      *****************************************************************
       2001111-NYUIN-EDIT-SEC          SECTION.
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   2 )
               IF    ( TNYUINRRK-DAY-RRKNUM (IDX4 DCACCT-DAY)
                                       >   ZERO )
                   EVALUATE    MASTER-TANICD
      *            月
                   WHEN   131
                       INITIALIZE          DTCHK013-REC
                       MOVE    TNYUINRRK-DAY-SHONUM   (IDX4 DCACCT-DAY)
                                           TO  DTCHK013-SHONUM
                       MOVE    WRK-BOMYMD  TO  DTCHK013-SRYSTYMD
                       MOVE    WRK-EOMYMD  TO  DTCHK013-SRYEDYMD
                       PERFORM 900-DTCHK013-READ-SEC
                       MOVE    DCACCT-SRYYMD (1:6)
                                           TO  DTCHK013-SRYYM
                       PERFORM 2001111-DTCHK013-EDIT-SEC
      *            入院時
                   WHEN   132
      *                算定日チェック用
                       INITIALIZE          DTCHK013-REC
                       MOVE    TNYUINRRK-DAY-SHONUM   (IDX4 DCACCT-DAY)
                                       TO  DTCHK013-SHONUM
                       MOVE    DCACCT-SRYYMD
                                       TO  DTCHK013-SRYSTYMD
                                           DTCHK013-SRYEDYMD
                                           DTCHK013-SRYYMD
                       MOVE    DTCHK013-REC
                                       TO  TMP013-REC
                       PERFORM 900-DTCHK013-READ-SEC
                       MOVE    TNYUINRRK-DAY-NYUINYMD (IDX4 DCACCT-DAY)
                                       TO  DTCHK013-NYUINYMD
                       PERFORM 2001111-DTCHK013-EDIT-SEC
      *
      *                算定回数チェック用
                       MOVE    TMP013-REC
                                       TO  DTCHK013-REC
                       MOVE    "00000000"
                                       TO  DTCHK013-SRYYMD
                       PERFORM 900-DTCHK013-READ-SEC
                       MOVE    TNYUINRRK-DAY-NYUINYMD (IDX4 DCACCT-DAY)
                                       TO  DTCHK013-NYUINYMD
                       PERFORM 2001111-DTCHK013-EDIT-SEC
      *            入院中
                   WHEN   133
                       INITIALIZE          DTCHK013-REC
                       MOVE    TNYUINRRK-DAY-SHONUM   (IDX4 DCACCT-DAY)
                                       TO  DTCHK013-SHONUM
                       MOVE    TNYUINRRK-DAY-NYUINYMD (IDX4 DCACCT-DAY)
                                       TO  DTCHK013-SRYSTYMD
                       MOVE    WRK-EOMYMD
                                       TO  DTCHK013-SRYEDYMD
                       PERFORM 900-DTCHK013-READ-SEC
                       MOVE    TNYUINRRK-DAY-NYUINYMD (IDX4 DCACCT-DAY)
                                       TO  DTCHK013-NYUINYMD
                       PERFORM 2001111-DTCHK013-EDIT-SEC
      *            退院時
                   WHEN   134
      *                算定日チェック用
                       INITIALIZE          DTCHK013-REC
                       MOVE    TNYUINRRK-DAY-SHONUM   (IDX4 DCACCT-DAY)
                                       TO  DTCHK013-SHONUM
                       MOVE    DCACCT-SRYYMD
                                       TO  DTCHK013-SRYSTYMD
                                           DTCHK013-SRYEDYMD
                                           DTCHK013-SRYYMD
                       MOVE    DTCHK013-REC
                                       TO  TMP013-REC
                       PERFORM 900-DTCHK013-READ-SEC
                       MOVE    TNYUINRRK-DAY-NYUINYMD (IDX4 DCACCT-DAY)
                                       TO  DTCHK013-NYUINYMD
                       MOVE    TNYUINRRK-DAY-TAIINYMD2(IDX4 DCACCT-DAY)
                                       TO  DTCHK013-TAIINYMD
                       PERFORM 2001111-DTCHK013-EDIT-SEC
      *
      *                算定回数チェック用
                       MOVE    TMP013-REC
                                       TO  DTCHK013-REC
                       MOVE    TNYUINRRK-DAY-NYUINYMD (IDX4 DCACCT-DAY)
                                       TO  DTCHK013-SRYSTYMD
                       MOVE    WRK-EOMYMD
                                       TO  DTCHK013-SRYEDYMD
                       MOVE    "00000000"
                                       TO  DTCHK013-SRYYMD
                       PERFORM 900-DTCHK013-READ-SEC
                       MOVE    TNYUINRRK-DAY-NYUINYMD (IDX4 DCACCT-DAY)
                                       TO  DTCHK013-NYUINYMD
                       MOVE    TNYUINRRK-DAY-TAIINYMD (IDX4 DCACCT-DAY)
                                       TO  DTCHK013-TAIINYMD
                       PERFORM 2001111-DTCHK013-EDIT-SEC
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           .
       2001111-NYUIN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者・単位別算定回数ファイル編集処理
      *****************************************************************
       2001111-DTCHK013-EDIT-SEC       SECTION.
      *
           IF    ( IDX4            =   ZERO )
               COMPUTE DTCHK013-KAISU-TOTAL
                   =   DTCHK013-KAISU-TOTAL
                   +   DCACCT-DAYCNT
               COMPUTE DTCHK013-KAISU1
                   =   DTCHK013-KAISU1
                   +   DCACCT-DAYCNT
               COMPUTE DTCHK013-DAY (DCACCT-DAY)
                   =   DTCHK013-DAY (DCACCT-DAY)
                   +   DCACCT-DAYCNT
           ELSE
               COMPUTE DTCHK013-KAISU-TOTAL
                   =   DTCHK013-KAISU-TOTAL
                   +   DCACCT-DAYCNT2 (IDX4)
               COMPUTE DTCHK013-KAISU1
                   =   DTCHK013-KAISU1
                   +   DCACCT-DAYCNT2 (IDX4)
               COMPUTE DTCHK013-DAY (DCACCT-DAY)
                   =   DTCHK013-DAY (DCACCT-DAY)
                   +   DCACCT-DAYCNT2 (IDX4)
           END-IF
      *
           IF    ( DTCHK013-NYUGAIKBN  =   SPACE )
            OR   ( DTCHK013-NYUGAIKBN  NOT =   ORCBSD01-NYUGAIKBN )
               MOVE    DCACCT-NYUGAIKBN
                                       TO  DTCHK013-NYUGAIKBN
           END-IF
      *
           IF    ( FLG-DTCHK013    =   ZERO )
               PERFORM 20011111-HKNCOMBI-EDIT-SEC
               REWRITE                     DTCHK013-REC
           ELSE
               MOVE    MASTER-TANINAME TO  DTCHK013-TANINAME
               MOVE    MASTER-KAISU    TO  DTCHK013-LIMIT
               MOVE    MASTER-TOKUREI  TO  DTCHK013-TOKUREI
               MOVE    WRK-TSUKISU     TO  DTCHK013-TSUKISU
               MOVE    WRK-JGNCNTERR   TO  DTCHK013-JGNCNTERR
               PERFORM 20011111-HKNCOMBI-EDIT-SEC
               WRITE                       DTCHK013-REC
           END-IF
      *
           .
       2001111-DTCHK013-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者・単位別算定回数ファイル編集処理
      *****************************************************************
       2001112-DTCHK013-EDIT-SEC       SECTION.
      *
           COMPUTE DTCHK013-KAISU-TOTAL
               =   DTCHK013-KAISU-TOTAL
               +   DCACCT-DAYCNT
      *
           COMPUTE DTCHK013-KAISU2
               =   DTCHK013-KAISU2
               +   DCACCT-DAYCNT
      *
           COMPUTE DTCHK013-DAY (DCACCT-DAY)
               =   DTCHK013-DAY (DCACCT-DAY)
               +   DCACCT-DAYCNT
      *
           IF    ( FLG-DTCHK013    =   ZERO )
               PERFORM 20011111-HKNCOMBI-EDIT-SEC
               REWRITE                     DTCHK013-REC
           ELSE
               MOVE    MASTER-TANINAME TO  DTCHK013-TANINAME
               MOVE    MASTER-KAISU    TO  DTCHK013-LIMIT
               MOVE    MASTER-TOKUREI  TO  DTCHK013-TOKUREI
               MOVE    WRK-JGNCNTERR   TO  DTCHK013-JGNCNTERR
               PERFORM 20011111-HKNCOMBI-EDIT-SEC
               WRITE                       DTCHK013-REC
           END-IF
      *
           .
       2001112-DTCHK013-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       20011111-HKNCOMBI-EDIT-SEC        SECTION.
      *
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL ( IDX6    >   CONST-HKNCOMBI-MAX )
      *
               IF    ( DTCHK013-HKNCOMBI (IDX6)
                                   =   ZERO
                                OR     DCACCT-HKNCOMBI )
                   IF    ( DTCHK013-HKNCOMBI (IDX6)  =   ZERO )
                       MOVE    DCACCT-HKNCOMBI
                                           TO  DTCHK013-HKNCOMBI (IDX6)
                       MOVE    IDX6        TO  DTCHK013-HKNCOMBI-MAX
                   END-IF
      *
                   MOVE    CONST-HKNCOMBI-MAX
                                           TO   IDX6
      *
               END-IF
      *
           END-PERFORM
      *
           .
       20011111-HKNCOMBI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    同日再入院情報退避領域初期処理
      *****************************************************************
       2100-TDOJITSU-EDIT-SEC          SECTION.
      *
           INITIALIZE                  TDOJITSU-AREA
      *
           PERFORM 900-PTNYUINRRK-KEY60-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( TDOJITSU-MAX       >=   CONST-TDOJITSU-MAX )
      *
               COMPUTE TDOJITSU-MAX = TDOJITSU-MAX     +   1
      *
               MOVE    PTNYUINRRK-NYUINYMD
                                   TO  TDOJITSU-NYUINYMD (TDOJITSU-MAX)
      *
               PERFORM 900-PTNYUINRRK-KEY60-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key60"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2100-TDOJITSU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴退避領域初期処理
      *****************************************************************
       2100-TNYUINRRK-EDIT-SEC         SECTION.
      *
           INITIALIZE                  TNYUINRRK-AREA
      *
           PERFORM 900-PTNYUINRRK-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( PTNYUINRRK-TAIINYMD <   WRK-BOMYMD )
      *
               MOVE    PTNYUINRRK-REC  TO  TNRRK-REC
               MOVE    ZERO            TO  IDXS
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   CONST-SHONUM-MAX )
                        OR   ( IDXS    >   ZERO )
      *
                   EVALUATE    TRUE
                   WHEN  ( TNYUINRRK-SHONUM (IDX1)
                                       =   PTNYUINRRK-SHONUM )
                       MOVE    IDX1    TO  IDXS
                   WHEN  ( TNYUINRRK-SHONUM (IDX1)     =   ZERO )
                       MOVE    PTNYUINRRK-SHONUM
                                       TO  TNYUINRRK-SHONUM (IDX1)
                       MOVE    PTNYUINRRK-NYUINYMD
                                       TO  TNYUINRRK-NYUINYMD (IDX1)
                       MOVE    PTNYUINRRK-TAIINYMD
                                       TO  TNYUINRRK-TAIINYMD (IDX1)
                       PERFORM 21001-TNYUINRRK-EDIT-SEC
                       MOVE    IDX1    TO  IDXS
                       MOVE    IDX1    TO  TNYUINRRK-SHONUM-MAX
                   END-EVALUATE
      *
               END-PERFORM
      *
               MOVE    TNRRK-REC        TO  PTNYUINRRK-REC
      *
               MOVE    ORCBSD01-SRYYM  TO  WRK-SRYYM
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
      *
                   MOVE    IDXD        TO  WRK-SRYDD
                   MOVE    1           TO  IDX1
                   IF    ( WRK-SRYYMD     >=   PTNYUINRRK-NYUINYMD )
                    AND  ( WRK-SRYYMD     <=   PTNYUINRRK-TAIINYMD )
                       IF    ( WRK-SRYYMD  =   PTNYUINRRK-NYUINYMD )
                        AND  ( PTNYUINRRK-NYUINYMD
                                       NOT =   PTNYUINRRK-TAIINYMD )
                           MOVE    PTNYUINRRK-NYUINYMD
                                       TO  WRK-NYUINYMD
                           PERFORM 21011-DOJITSU-SAINYUIN-CHK-SEC
                           IF    ( FLG-DOJITSU =   1 )
                               MOVE    2   TO  IDX1
                           END-IF
                       END-IF
      *
                       MOVE    PTNYUINRRK-SHONUM
                                   TO TNYUINRRK-DAY-SHONUM (IDX1 IDXD)
                       MOVE    PTNYUINRRK-NYUINYMD
                                   TO TNYUINRRK-DAY-NYUINYMD (IDX1 IDXD)
                                      TNYUINRRK-DAY-NYUINYMD2(IDX1 IDXD)
                       MOVE    PTNYUINRRK-TAIINYMD
                                   TO TNYUINRRK-DAY-TAIINYMD (IDX1 IDXD)
                                      TNYUINRRK-DAY-TAIINYMD2(IDX1 IDXD)
      *
                       IF    ( IDXS    >  ZERO )
                           MOVE    TNYUINRRK-NYUINYMD (IDXS)
                                   TO TNYUINRRK-DAY-NYUINYMD (IDX1 IDXD)
                           MOVE    TNYUINRRK-TAIINYMD (IDXS)
                                   TO TNYUINRRK-DAY-TAIINYMD (IDX1 IDXD)
                       END-IF
      *
                       MOVE    PTNYUINRRK-RRKNUM
                                   TO TNYUINRRK-DAY-RRKNUM (IDX1 IDXD)
      *
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-PTNYUINRRK-KEY17-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2100-TNYUINRRK-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴退避領域初期処理
      *****************************************************************
       21001-TNYUINRRK-EDIT-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX2
      *
           PERFORM 900-PTNYUINRRK-KEY38-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( TNYUINRRK-RRK-MAX (IDX1)
                                              >=   CONST-RRK-MAX )
      *
               COMPUTE IDX2    =   IDX2    +   1
               MOVE    IDX2    TO  TNYUINRRK-RRK-MAX (IDX1)
      *
               IF    ( PTNYUINRRK-JTIKBN   NOT =   "5" )
                   IF    ( PTNYUINRRK-NYUINYMD
                                   <   TNYUINRRK-NYUINYMD (IDX1) )
                       MOVE    PTNYUINRRK-NYUINYMD
                               TO  TNYUINRRK-NYUINYMD (IDX1)
                   END-IF
               END-IF
      *
               IF    ( PTNYUINRRK-TAIINYMD
                               >   TNYUINRRK-TAIINYMD (IDX1) )
                   MOVE    PTNYUINRRK-TAIINYMD
                               TO  TNYUINRRK-TAIINYMD (IDX1)
               END-IF
      *
               MOVE    PTNYUINRRK-RRKNUM
                               TO  TNYUINRRK-RRKNUM   (IDX1 IDX2)
               MOVE    PTNYUINRRK-NYUINYMD
                               TO  TNYUINRRK-SRYSTYMD (IDX1 IDX2)
               MOVE    PTNYUINRRK-TAIINYMD
                               TO  TNYUINRRK-SRYEDYMD (IDX1 IDX2)
               MOVE    PTNYUINRRK-JTIKBN
                               TO  TNYUINRRK-JTIKBN   (IDX1 IDX2)
      *
               IF    ( PTNYUINRRK-NYUINYMD NOT =   PTNYUINRRK-TAIINYMD )
                   MOVE    PTNYUINRRK-NYUINYMD
                                       TO  WRK-NYUINYMD
                   PERFORM 21011-DOJITSU-SAINYUIN-CHK-SEC
                   IF    ( FLG-DOJITSU =   1 )
                       MOVE    1       TO  TNYUINRRK-DOJITSU (IDX1 IDX2)
                   END-IF
               END-IF
      *
               PERFORM 900-PTNYUINRRK-KEY38-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       21001-TNYUINRRK-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    同日再入院チェック処理
      *****************************************************************
       21011-DOJITSU-SAINYUIN-CHK-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-DOJITSU
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL ( IDX3    >   TDOJITSU-MAX )
                    OR   ( FLG-DOJITSU     =   1 )
      *
               IF    ( WRK-NYUINYMD    =   TDOJITSU-NYUINYMD (IDX3))
                   MOVE    1       TO  FLG-DOJITSU
               END-IF
      *
           END-PERFORM
      *
           .
       21011-DOJITSU-SAINYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    算定回数編集処理（点数マスタより）
      *****************************************************************
       20012-KAISU-EDIT-SEC            SECTION.
      *
           MOVE    DCSRY-SRYCD         TO  SEL-SRYCD
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               PERFORM 900-TENSUPLUS-KEY-SEL-SEC
               IF    ( FLG-TENSUPLUS   =   ZERO )
                AND  ( TNSP-SANTEIRRKKBN   =   1 )
                   MOVE    TENSUPLUS-REC
                                   TO  TNSCHK-AREA
                   PERFORM 200121-DTCHK013-EDIT-SEC
               ELSE
                   INITIALIZE              TNSCHK-AREA
                   MOVE    TNS-JGNCNT
                                       TO  TNSCHK-JGNCNT
                   MOVE    TNS-JGNCNT1D
                                       TO  TNSCHK-JGNCNT1D
                   MOVE    TNS-JGNCNT1W
                                       TO  TNSCHK-JGNCNT1W
                   MOVE    TNS-JGNCNTETCM
                                       TO  TNSCHK-JGNCNTETCM
                   MOVE    TNS-JGNCNTETC
                                       TO  TNSCHK-JGNCNTETC
                   MOVE    TNS-JGNCNTERR
                                       TO  TNSCHK-JGNCNTERR
                   PERFORM 200121-DTCHK013-EDIT-SEC
               END-IF
           END-IF
      *
           .
       20012-KAISU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    患者・単位別算定回数ファイル編集処理
      *****************************************************************
       200121-DTCHK013-EDIT-SEC        SECTION.
      *
           MOVE    "1"                 TO  WRK-MSTKBN
           MOVE    TNSCHK-JGNCNTERR    TO  WRK-JGNCNTERR
           MOVE    ZERO                TO  IDX4
      *
      *    月
           IF    ( TNSCHK-JGNCNT          >  ZERO )
               MOVE    ZERO            TO  WRK-TSUKISU
               INITIALIZE                  MASTER-AREA
               MOVE    131             TO  MASTER-TANICD
               MOVE    "月"            TO  MASTER-TANINAME
               MOVE    TNSCHK-JGNCNT     TO  MASTER-KAISU
      *
               IF    ( DCACCT-SRYKBN   =   "90" OR "92" )
                   PERFORM 2001111-NYUIN-EDIT-SEC
               ELSE
      *
                   INITIALIZE                  DTCHK013-REC
                   MOVE    WRK-BOMYMD      TO  DTCHK013-SRYSTYMD
                   MOVE    WRK-EOMYMD      TO  DTCHK013-SRYEDYMD
                   PERFORM 900-DTCHK013-READ-SEC
                   MOVE    DCACCT-SRYYMD (1:6)
                                           TO  DTCHK013-SRYYM
                   PERFORM 2001111-DTCHK013-EDIT-SEC
               END-IF
           END-IF
      *
      *    日
           IF    ( TNSCHK-JGNCNT1D       >   ZERO )
               MOVE    ZERO            TO  WRK-TSUKISU
               INITIALIZE                  MASTER-AREA
               MOVE    121             TO  MASTER-TANICD
               MOVE    "日"            TO  MASTER-TANINAME
               MOVE    TNSCHK-JGNCNT1D   TO  MASTER-KAISU
      *
               INITIALIZE                  DTCHK013-REC
               MOVE    DCACCT-SRYYMD   TO  DTCHK013-SRYSTYMD
                                           DTCHK013-SRYEDYMD
                                           DTCHK013-SRYYMD
               PERFORM 900-DTCHK013-READ-SEC
               PERFORM 2001111-DTCHK013-EDIT-SEC
           END-IF
      *
      *    週
           IF    ( TNSCHK-JGNCNT1W       >   ZERO )
               MOVE    ZERO            TO  WRK-TSUKISU
               INITIALIZE                  MASTER-AREA
               MOVE    138             TO  MASTER-TANICD
               MOVE    "週"            TO  MASTER-TANINAME
               MOVE    TNSCHK-JGNCNT1W   TO  MASTER-KAISU
      *
               INITIALIZE              DTCHK013-REC
               MOVE    DCACCT-SRYYMD   TO  WRK-SYMD
               PERFORM 800-DOW-SEC
               MOVE    WRK-TW-SUNDAY   TO  DTCHK013-SRYSTYMD
               MOVE    WRK-TW-SATDAY   TO  DTCHK013-SRYEDYMD
               PERFORM 900-DTCHK013-READ-SEC
               MOVE    WRK-WEEK        TO  DTCHK013-WEEK
               PERFORM 2001111-DTCHK013-EDIT-SEC
           END-IF
      *
      *    患者当りor月フリー
           IF    ( TNSCHK-JGNCNTETCM     >   ZERO )
            AND  ( TNSCHK-JGNCNTETC      >   ZERO )
               IF    ( TNSCHK-JGNCNTETCM =   99 )
                   MOVE    ZERO        TO  WRK-TSUKISU
                   INITIALIZE              MASTER-AREA
                   MOVE    53          TO  MASTER-TANICD
                   MOVE    "患者当り"  TO  MASTER-TANINAME
                   MOVE    TNSCHK-JGNCNTETC
                                       TO  MASTER-KAISU
      *
                   INITIALIZE              DTCHK013-REC
                   MOVE    "00000000"  TO  DTCHK013-SRYSTYMD
                   MOVE    WRK-EOMYMD  TO  DTCHK013-SRYEDYMD
                   PERFORM 900-DTCHK013-READ-SEC
                   PERFORM 2001111-DTCHK013-EDIT-SEC
               ELSE
                   MOVE    TNSCHK-JGNCNTETCM
                                       TO  WRK-TSUKISU
                   INITIALIZE              MASTER-AREA
                   MOVE    901         TO  MASTER-TANICD
                   MOVE    SPACE       TO  WRK-KANACHK-STR
                   IF    ( WRK-TSUKISU >=  10 )
                       MOVE    WRK-TSUKISU TO  WRK-TSUKISUZ2
                       STRING  WRK-TSUKISUZ2   DELIMITED BY SIZE
                               "月"            DELIMITED BY SIZE
                       INTO    WRK-KANACHK-STR
                       END-STRING
                   ELSE
                       MOVE    WRK-TSUKISU TO  WRK-TSUKISUZ1
                       STRING  WRK-TSUKISUZ1   DELIMITED BY SIZE
                               "月"            DELIMITED BY SIZE
                       INTO    WRK-KANACHK-STR
                       END-STRING
                   END-IF
                   PERFORM 800-ZENKAKU-SEC
                   MOVE    WRK-KANACHK-STR     TO  MASTER-TANINAME
                   MOVE    TNSCHK-JGNCNTETC    TO  MASTER-KAISU
      *
                   INITIALIZE              DTCHK013-REC
                   MOVE    WRK-BOMYMD      TO  WRK-SYMD
                   MOVE    "2"             TO  WRK-ZOGENPTN
                   COMPUTE WRK-ZOGEN   =   WRK-TSUKISU *   -1  +   1
                   PERFORM 800-CALENDAR-SEC
                   MOVE    WRK-SYMD        TO  DTCHK013-SRYSTYMD
                   MOVE    WRK-EOMYMD      TO  DTCHK013-SRYEDYMD
                   PERFORM 900-DTCHK013-READ-SEC
                   MOVE    DCACCT-SRYYMD (1:6)
                                           TO  DTCHK013-SRYYM
                   PERFORM 2001111-DTCHK013-EDIT-SEC
               END-IF
           END-IF
      *
           .
       200121-DTCHK013-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    過去算定回数編集処理
      *****************************************************************
       2001-KAKO-KAISU-EDIT-SEC        SECTION.
      *
           OPEN  I-O   DTCHK013-FILE
      *
           PERFORM 901-DTCHK013-READ-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK013    NOT =   ZERO )
      *
               MOVE    SPACE   TO  WRK-OUTYMD
      *
               MOVE    ZERO    TO  WRK-KAISU
      *
               EVALUATE    DTCHK013-TANICD
      *        日
               WHEN   121
      *        月
               WHEN   131
                   CONTINUE
      *        入院初日
               WHEN   132
      *        退院時
               WHEN   134
                   IF    ( DTCHK013-SRYSTYMD (1:6) <   ORCBSD01-SRYYM )
                    AND  ( DTCHK013-SRYYMD         =   "00000000" )
                       PERFORM 20011-KAKO-KAISU-EDIT-SEC
                       IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
                           PERFORM 20012-KAKO-KAISU-EDIT-SEC
                       END-IF
                   END-IF
      *        患者当り
               WHEN    53
      *        入院中
               WHEN   133
      *        週
               WHEN   138
      *        ２週
               WHEN   142
      *        ２月
               WHEN   143
      *        ３月
               WHEN   144
      *        ４月
               WHEN   145
      *        ６月
               WHEN   146
      *        １２月
               WHEN   147
      *        ５年
               WHEN   148
      *        月（フリー入力）
               WHEN   901
                   IF    ( DTCHK013-SRYSTYMD (1:6) <   ORCBSD01-SRYYM )
                       PERFORM 20011-KAKO-KAISU-EDIT-SEC
                       IF    ( ORCBSD01-NYUGAIKBN  =   "1" )
                           PERFORM 20012-KAKO-KAISU-EDIT-SEC
                       END-IF
                   END-IF
               END-EVALUATE
      *
               COMPUTE DTCHK013-KAISU-TOTAL
                   =   DTCHK013-KAISU-TOTAL
                   +   WRK-KAISU
      *
               COMPUTE DTCHK013-KAISU2
                   =   DTCHK013-KAISU2
                   +   WRK-KAISU
      *
      *
               IF    ( DTCHK013-OUTYMD     =   SPACE )
                OR   ( WRK-OUTYMD          >   DTCHK013-OUTYMD )
                   MOVE    WRK-OUTYMD      TO  DTCHK013-OUTYMD
               END-IF
      *
               REWRITE DTCHK013-REC
      *
               PERFORM 901-DTCHK013-READ-SEC
      *
           END-PERFORM
      *
           CLOSE DTCHK013-FILE
      *
           .
       2001-KAKO-KAISU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    過去算定回数編集処理
      *****************************************************************
       20011-KAKO-KAISU-EDIT-SEC       SECTION.
      *
           PERFORM 900-SRYACT-KEY20-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
      *
               PERFORM 900-SRYACCT-KEY57-SEL-SEC
      *
               MOVE    ACCT-PTID       TO  WRK-PTID
               MOVE    ACCT-HKNCOMBI   TO  WRK-HKNCOMBINUM
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
      *
               PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
                        OR   ( SRY-ZAINUM  NOT =   ACCT-ZAINUM )
      *
                   IF  ( ACCT-SRYKBN   =   "95" OR "96"
                                                OR "98" )
                    OR ( ACCT-HKNCOMBI =   9999 )
                    OR ( COMB-HKNNUM   =   "971" OR "972" OR "973" )
                    OR ( COMB-KOHHKNNUM (1)  =   "970" )
                    OR ( ACCT-JIHOKBN  =   "1" )
      *
                       CONTINUE
                   ELSE
                       PERFORM VARYING IDXA    FROM    1   BY  1
                               UNTIL ( IDXA    >   5 )
                                OR   ( SRY-SRYCD (IDXA)    =   SPACE )
      *
                           IF    ( DTCHK013-SRYCD  =   SRY-SRYCD(IDXA))
                               IF    ( DTCHK013-TANICD =   132
                                                        OR 133
                                                        OR 134 )
                                   PERFORM 200112-KAKO-KAISU-EDIT-SEC
                               ELSE
                                   PERFORM 200111-KAKO-KAISU-EDIT-SEC
                               END-IF
                           END-IF
      *
                       END-PERFORM
                   END-IF
      *
                   PERFORM 900-SRYACT-KEY20-FET-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-KAKO-KAISU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    過去算定回数編集処理
      *****************************************************************
       200111-KAKO-KAISU-EDIT-SEC      SECTION.
      *
           MOVE    ACCT-SRYYM      TO  WRK-SRYYM
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    IDXD            TO  WRK-SRYDD
               IF    ( DTCHK013-SRYSTYMD  <=   WRK-SRYYMD )
                AND  ( ACCT-DAY (1 IDXD)   >   ZERO )
      *
                   COMPUTE WRK-KAISU
                       =   WRK-KAISU   +   ACCT-DAY (1 IDXD)
      *
                   IF    ( WRK-OUTYMD  =   SPACE )
                    OR   ( WRK-SRYYMD  >   WRK-OUTYMD )
                       MOVE    WRK-SRYYMD  TO  WRK-OUTYMD
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200111-KAKO-KAISU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    過去算定回数編集処理
      *****************************************************************
       200112-KAKO-KAISU-EDIT-SEC      SECTION.
      *
           MOVE    ACCT-SRYYM      TO  WRK-SRYYM
           PERFORM VARYING IDXS    FROM    1   BY  1
                   UNTIL ( IDXS    >   TNYUINRRK-SHONUM-MAX )
      *
               IF    ( DTCHK013-SHONUM =   TNYUINRRK-SHONUM (IDXS))
      *
                   MOVE    TNYUINRRK-RRK-MAX (IDXS)
                                       TO  WRK-RRK-MAX
      *
                   PERFORM VARYING IDXD    FROM    1   BY  1
                           UNTIL ( IDXD    >   31 )
      *
                       IF    ( ACCT-DAY (1 IDXD)   >   ZERO )
                           MOVE    IDXD    TO   WRK-SRYDD
                           PERFORM 2001121-KAKO-KAISU-EDIT-SEC
                       END-IF
      *
                   END-PERFORM
      *
                   MOVE    TNYUINRRK-SHONUM-MAX    TO  IDXS
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200112-KAKO-KAISU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    過去算定回数編集処理
      *****************************************************************
       2001121-KAKO-KAISU-EDIT-SEC     SECTION.
      *
           PERFORM VARYING IDXR    FROM    1   BY  1
                   UNTIL ( IDXR    >   WRK-RRK-MAX )
      *
               IF    ( DTCHK013-SRYSTYMD  <=   WRK-SRYYMD )
                AND  ( TNYUINRRK-SRYSTYMD (IDXS IDXR)
                                          <=   WRK-SRYYMD )
                AND  ( TNYUINRRK-SRYEDYMD (IDXS IDXR)
                                          >=   WRK-SRYYMD )
                   IF    ( TNYUINRRK-SRYSTYMD (IDXS IDXR)
                                           =   WRK-SRYYMD )
                    AND  ( TNYUINRRK-DOJITSU  (IDXS IDXR)
                                           =   1 )
                       MOVE    3       TO  IDXK
                       PERFORM 20011211-KAKO-KAISU-EDIT-SEC
                   ELSE
                       MOVE    2       TO  IDXK
                       PERFORM 20011211-KAKO-KAISU-EDIT-SEC
                   END-IF
      *
                   MOVE    WRK-RRK-MAX TO  IDXR
      *
               END-IF
      *
           END-PERFORM
      *
           .
       2001121-KAKO-KAISU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    過去算定回数編集処理
      *****************************************************************
       20011211-KAKO-KAISU-EDIT-SEC     SECTION.
      *
           IF    ( ACCT-DAY (IDXK IDXD)   >   ZERO )
      *
               COMPUTE WRK-KAISU
                   =   WRK-KAISU   +   ACCT-DAY (IDXK IDXD)
      *
               IF    ( WRK-OUTYMD  =   SPACE )
                OR   ( WRK-SRYYMD  >   WRK-OUTYMD )
                   MOVE    WRK-SRYYMD  TO  WRK-OUTYMD
               END-IF
      *
           END-IF
      *
           .
       20011211-KAKO-KAISU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    過去算定回数編集処理
      *****************************************************************
       20012-KAKO-KAISU-EDIT-SEC      SECTION.
      *
           PERFORM 900-NSRYSRH-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NSRYSRH NOT =   ZERO )
      *
               MOVE    NSRYSRH-PTID       TO  WRK-PTID
               MOVE    NSRYSRH-HKNCOMBINUM TO WRK-HKNCOMBINUMX
               PERFORM 900-HKNCOMBI-KEY-SEL-SEC
      *
               IF  ( COMB-HKNNUM         =   "971" OR "972" OR "973" )
                OR ( COMB-KOHHKNNUM (1)  =   "970" )
                   CONTINUE
               ELSE
      *
                   PERFORM 900-NRRKSRH-KEY3-SEL-SEC
      *
                   PERFORM UNTIL ( FLG-NRRKSRH NOT =   ZERO )
      *
                       IF    ( DTCHK013-TANICD =   132 OR 133 OR 134 )
                           PERFORM 200122-KAKO-KAISU-EDIT-SEC
                       ELSE
                           PERFORM 200121-KAKO-KAISU-EDIT-SEC
                       END-IF
      *
                       PERFORM 900-NRRKSRH-KEY3-FET-SEC
      *
                   END-PERFORM
      *
                   MOVE    "tbl_nrrksrh"   TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
      *
               END-IF
      *
               PERFORM 900-NSRYSRH-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nsrysrh"   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20012-KAKO-KAISU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    過去算定回数編集処理
      *****************************************************************
       200121-KAKO-KAISU-EDIT-SEC      SECTION.
      *
           IF    ( DTCHK013-SRYSTYMD  <=   NRRKSRH-SRYYMD )
      *
               COMPUTE WRK-KAISU   =   WRK-KAISU   +   1
      *
               IF    ( WRK-OUTYMD  =   SPACE )
                OR   ( NRRKSRH-SRYYMD  >   WRK-OUTYMD )
                   MOVE    NRRKSRH-SRYYMD
                                   TO  WRK-OUTYMD
               END-IF
      *
           END-IF
      *
           .
       200121-KAKO-KAISU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    過去算定回数編集処理
      *****************************************************************
       200122-KAKO-KAISU-EDIT-SEC      SECTION.
      *
           PERFORM VARYING IDXS    FROM    1   BY  1
                   UNTIL ( IDXS    >   TNYUINRRK-SHONUM-MAX )
      *
               IF    ( DTCHK013-SHONUM =   TNYUINRRK-SHONUM (IDXS))
      *
                   MOVE    TNYUINRRK-RRK-MAX (IDXS)
                                       TO  WRK-RRK-MAX
      *
                   PERFORM VARYING IDXR    FROM    1   BY  1
                           UNTIL ( IDXR    >   WRK-RRK-MAX )
      *
                       IF    ( DTCHK013-SRYSTYMD
                                          <=   NRRKSRH-SRYYMD )
                            AND  ( TNYUINRRK-RRKNUM   (IDXS IDXR)
                                           =   NRRKSRH-RRKNUM )
      *
                           COMPUTE WRK-KAISU
                               =   WRK-KAISU   +   1
      *
                           IF    ( WRK-OUTYMD  =   SPACE )
                            OR   ( NRRKSRH-SRYYMD  >   WRK-OUTYMD )
                               MOVE    NRRKSRH-SRYYMD
                                                   TO  WRK-OUTYMD
                           END-IF
      *
                           MOVE    WRK-RRK-MAX     TO  IDXR
      *
                       END-IF
      *
                   END-PERFORM
      *
                   MOVE    TNYUINRRK-SHONUM-MAX    TO  IDXS
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200122-KAKO-KAISU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    参照マスタ編集処理
      *****************************************************************
       2001-REFMST-EDIT-SEC            SECTION.
      *
           OPEN    INPUT               DTCHK003-FILE
           OPEN    I-O                 DTCHK018-FILE
           OPEN    I-O                 DTCHK022-FILE
      *
           MOVE    ZERO            TO  KEY-PTID
      *
           PERFORM 900-DTCHK003-READ-SEC
      *
           PERFORM UNTIL ( FLG-DTCHK003    NOT =   ZERO )
      *
               MOVE    DTCHK003-PTID   TO  KEY-PTID
      *
               PERFORM 901-DCACCT-START-SEC
      *
               PERFORM UNTIL ( FLG-DCACCT      NOT =   ZERO )
                        OR   ( FLG-END         NOT =   ZERO )
      *
                   PERFORM 901-DCSRY-START-SEC
      *
                   PERFORM UNTIL ( FLG-DCSRY   NOT =   ZERO )
                            OR   ( FLG-END     NOT =   ZERO )
      *
                       PERFORM 20011-CHKMST-EDIT-SEC
      *
                       IF    ( ORCBSD01-CHKKBN (21)    =   "2" )
                           PERFORM 20011-RKISAI-EDIT-SEC
                       END-IF
      *
                       PERFORM 901-DCSRY-READ-SEC
      *
                   END-PERFORM
      *
                   PERFORM 901-DCACCT-READ-SEC
      *
               END-PERFORM
      *
               PERFORM UNTIL ( FLG-DTCHK003    NOT =   ZERO )
                        OR   ( DTCHK003-PTID   NOT =   KEY-PTID )
      *
                   PERFORM 900-DTCHK003-READ-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           CLOSE   DTCHK003-FILE
                   DTCHK018-FILE
                   DTCHK022-FILE
      *
           .
       2001-REFMST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    併算定チェックマスタ編集処理
      *****************************************************************
       20011-CHKMST-EDIT-SEC           SECTION.
      *
           INITIALIZE                  SEL-CHK-AREA
           MOVE    "#"             TO  SEL-CHKKBN
           PERFORM 900-DTCHK018-READ-SEC
           IF    ( FLG-DTCHK018    =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE                  SEL-CHK-AREA
               MOVE    "#"             TO  SEL-CHKKBN
               PERFORM 20011211-CHKMST-EDIT-SEC
      *
               MOVE    DCSRY-SRYCD TO  SEL-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NYUGAITEKKBN
                                   TO  WRK-NYUGAITEKKBN
      *
               MOVE    "4"         TO  SEL-CHKKBN
               PERFORM 200111-CHKMST-EDIT-SEC
      *
               MOVE    "5"         TO  SEL-CHKKBN
               PERFORM 200111-CHKMST-EDIT-SEC
      *
               MOVE    "A"         TO  SEL-CHKKBN
               PERFORM 200111-CHKMST-EDIT-SEC
      *
               MOVE    "B"         TO  SEL-CHKKBN
               PERFORM 200111-CHKMST-EDIT-SEC
      *
               IF    ( SYS-1101-SRYHEIMSTKBN   =   "1" )
                   MOVE    "5"         TO  SEL-CHKKBN
                   PERFORM 200113-CHKMST-EDIT-SEC
      *
                   MOVE    "A"         TO  SEL-CHKKBN
                   PERFORM 200113-CHKMST-EDIT-SEC
      *
                   MOVE    "B"         TO  SEL-CHKKBN
                   PERFORM 200113-CHKMST-EDIT-SEC
               END-IF
      *
           END-IF
      *
           .
       20011-CHKMST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    併算定チェックマスタ編集処理
      *****************************************************************
       200111-CHKMST-EDIT-SEC           SECTION.
      *
           MOVE    ZERO            TO  CNT-TEKCD
      *
           PERFORM 900-CHK-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
      *
               COMPUTE CNT-TEKCD = CNT-TEKCD   +   1
      *
               MOVE    CHK-REC     TO  DTCHK018-REC
      *
               MOVE    "00000000"  TO  DTCHK018-YUKOSTYMD
               MOVE    "99999999"  TO  DTCHK018-YUKOEDYMD
               MOVE    SPACE       TO  DTCHK018-OPID
               MOVE    CNT-TEKCD   TO  DTCHK018-RENNUM
      *
               WRITE   DTCHK018-REC
      *
               PERFORM 900-CHK-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk"       TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200111-CHKMST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レセ記載マスタ編集処理
      *****************************************************************
       20011-RKISAI-EDIT-SEC           SECTION.
      *
           PERFORM 900-DTCHK022-READ-SEC
           IF    ( FLG-DTCHK022    =   ZERO )
               CONTINUE
           ELSE
               PERFORM 200111-RKISAI-EDIT-SEC
           END-IF
      *
           .
       20011-RKISAI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レセ記載マスタ編集処理
      *****************************************************************
       200111-RKISAI-EDIT-SEC          SECTION.
      *
           INITIALIZE                  DTCHK022-REC
           MOVE    DCSRY-SRYCD     TO  DTCHK022-ISAM-SRYCD
           WRITE   DTCHK022-REC
      *
           PERFORM 900-RECEKISAI-KEY5-SEL-SEC
      *
           PERFORM UNTIL ( FLG-RKISAI  NOT =   ZERO )
      *
               INITIALIZE                  DTCHK022-REC
      *
               MOVE    RKISAI-SRYCD    TO  DTCHK022-ISAM-SRYCD
               MOVE    RKISAI-ITEMNO   TO  DTCHK022-ISAM-ITEMNO
               MOVE    RKISAI-EDANO    TO  DTCHK022-ISAM-EDANO
               MOVE    RKISAI-RENNUM   TO  DTCHK022-ISAM-RENNUM
      *
               MOVE    RKISAI-REC      TO  DTCHK022-RKISAI-AREA
      *
               WRITE   DTCHK022-REC
      *
               PERFORM 900-RECEKISAI-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_recekisai" TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200111-RKISAI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    併算定チェックマスタ編集処理（電子点数表より）
      *****************************************************************
       20011211-CHKMST-EDIT-SEC       SECTION.
      *
           INITIALIZE                  DTCHK018-REC
           MOVE    SPA-HOSPNUM     TO  DTCHK018-HOSPNUM
           MOVE    SEL-CHKKBN      TO  DTCHK018-CHKKBN
           MOVE    DCSRY-SRYCD     TO  DTCHK018-SRYCD
           MOVE    "00000000"      TO  DTCHK018-YUKOSTYMD
           MOVE    "99999999"      TO  DTCHK018-YUKOEDYMD
           MOVE    SEL-CDKBN       TO  DTCHK018-CDKBN
           MOVE    SEL-CD          TO  DTCHK018-CD
           MOVE    SEL-DATAKBN     TO  DTCHK018-DATAKBN
      *
           MOVE    SEL-CD          TO  SEL-SRYCD
           PERFORM 900-DTCHK019-READ-SEC
           IF    ( FLG-DTCHK019    =   ZERO )
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( TNS-NYUGAITEKKBN    NOT =   ZERO )
                  AND( WRK-NYUGAITEKKBN    NOT =   ZERO )
                  AND( TNS-NYUGAITEKKBN    NOT =   WRK-NYUGAITEKKBN )
                   MOVE    8       TO  DTCHK018-CHKERR
               END-IF
           ELSE
                   MOVE    8       TO  DTCHK018-CHKERR
           END-IF
      *
      *
           WRITE   DTCHK018-REC
      *
           .
       20011211-CHKMST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    併算定チェックマスタ編集処理（電子点数表より）
      *****************************************************************
       200113-CHKMST-EDIT-SEC          SECTION.
      *
           EVALUATE     SEL-CHKKBN
           WHEN  "5"
               MOVE    "tbl_etensu_3_2"    TO  WRK-TABLE
               MOVE    "key3"              TO  WRK-PATHNAME
           WHEN  "A"
               MOVE    "tbl_etensu_3_1"    TO  WRK-TABLE
               MOVE    "key3"              TO  WRK-PATHNAME
           WHEN  "B"
               MOVE    "tbl_etensu_3_3"    TO  WRK-TABLE
               MOVE    "key3"              TO  WRK-PATHNAME
           END-EVALUATE
      *
           PERFORM 900-ETENSU3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-ETENSU3 NOT =   ZERO )
      *
               MOVE    "2"             TO  SEL-CDKBN
               MOVE    ETENSU3-SRYCD2  TO  SEL-CD
               PERFORM 900-DTCHK018-READ-SEC
               IF    ( FLG-DTCHK018    NOT =   ZERO )
                   MOVE    "B"         TO  SEL-DATAKBN
                   PERFORM 20011211-CHKMST-EDIT-SEC
               END-IF
      *
               PERFORM 900-ETENSU3-FET-SEC
      *
           END-PERFORM
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200113-CHKMST-EDIT-EXT.
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
           CLOSE                       DCACCT-FILE
                                       DCSRY-FILE
                                       DTCHK014-FILE
                                       DTCHK015-FILE
                                       DTCHK016-FILE
                                       DTCHK017-FILE
                                       DTCHK019-FILE
      *
           IF      LNK-PRTKANRI-TBL-KEY  NOT =  "RECEPTX"
      *    ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
      *
               PERFORM 900-DBCOMMIT-SEC
           END-IF
      *
           DISPLAY "*** ORCDTCHK011 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
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
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    曜日取得処理(1:日曜日...7:土曜日）
      *****************************************************************
       800-DOW-SEC                     SECTION.
      *
           MOVE    ZERO            TO  WRK-DOW
           MOVE    ZERO            TO  WRK-WEEK
           MOVE    SPACE           TO  WRK-TW-SUNDAY
                                       WRK-TW-SATDAY
                                       WRK-LW-SUNDAY
                                       WRK-NW-SUNDAY
      *
           MOVE    WRK-SYMD        TO  WRK-KEISANYMD
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY3-AREA
           MOVE    "31"            TO  LNK-DAY3-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY3-AREA
           IF    ( LNK-DAY3-YOBI   =   7 )
               COMPUTE WRK-DOW     =   1
           ELSE
               COMPUTE WRK-DOW     =   LNK-DAY3-YOBI + 1
           END-IF
      *
           COMPUTE WRK-WEEK = ( WRK-KEISANDD - WRK-DOW + 13 ) / 7
      *
           MOVE    WRK-KEISANYMD   TO  WRK-SYMD
           MOVE    "1"             TO  WRK-ZOGENPTN
           COMPUTE WRK-ZOGEN = ( WRK-DOW * -1 ) + 1
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-SYMD        TO  WRK-TW-SUNDAY
      *
           MOVE    WRK-KEISANYMD   TO  WRK-SYMD
           MOVE    "1"             TO  WRK-ZOGENPTN
           COMPUTE WRK-ZOGEN = 7 - WRK-DOW
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-SYMD        TO  WRK-TW-SATDAY
      *
           MOVE    WRK-KEISANYMD   TO  WRK-SYMD
           MOVE    "1"             TO  WRK-ZOGENPTN
           COMPUTE WRK-ZOGEN = ( WRK-DOW * -1 ) + 1 - 7
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-SYMD        TO  WRK-LW-SUNDAY
      *
           MOVE    WRK-KEISANYMD   TO  WRK-SYMD
           MOVE    "1"             TO  WRK-ZOGENPTN
           COMPUTE WRK-ZOGEN = 7 - WRK-DOW + 1
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-SYMD        TO  WRK-NW-SUNDAY
      *
           MOVE    WRK-KEISANYMD   TO  WRK-SYMD
      *
           .
       800-DOW-EXT.
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
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE   "2"              TO  KANACHK-SYORI
           MOVE  500               TO  KANACHK-MAX-CNT
           MOVE  WRK-KANACHK-STR   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-STR
      *
           .
       800-ZENKAKU-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
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
           MOVE    ZERO                TO  FLG-DTCHK003
      *
           READ    DTCHK003-FILE
           AT  END
               MOVE    1               TO  FLG-DTCHK003
           END-READ
           IF    ( STS-COMMON  >   10 )
               MOVE    1               TO  FLG-DTCHK003
           END-IF
      *
           .
       900-DTCHK003-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療会計START処理
      *****************************************************************
       900-DCACCT-START-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-DCACCT
      *
           INITIALIZE                      DCACCT-KEY
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
           MOVE    ZERO                TO  FLG-DCACCT
      *
           PERFORM UNTIL ( FLG-DCACCT      NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               READ    DCACCT-FILE     NEXT
               AT  END
                       MOVE    1           TO  FLG-DCACCT
               NOT AT  END
      *
                   IF    ( KEY-PTID    NOT =   DCACCT-PTID )
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
                           IF    ( DCACCT-HKNCOMBI =   9999 )
                            OR   ( DCACCT-HKNNUM   =   "971" )
                            OR   ( DCACCT-HKNNUM   =   "973" )
                            OR   ( DCACCT-HKNNUM   =   "975" )
                            OR   ( DCACCT-KOHNUM   =   "970" )
                            OR   ( DCACCT-JIHOKBN  =   "1" )
                               MOVE   ZERO    TO   FLG-LOOP-END
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
      *    データチェック診療行為START処理
      *****************************************************************
       900-DCSRY-START-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-DCSRY
      *
           INITIALIZE                      DCSRY-KEY
           MOVE    DCACCT-PTID         TO  DCSRY-PTID
           MOVE    DCACCT-ZAINUM       TO  DCSRY-ZAINUM
           MOVE    DCACCT-ACCTKBN      TO  DCSRY-ACCTKBN
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
       900-DCSRY-START-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療行為読込処理
      *****************************************************************
       900-DCSRY-READ-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-LOOP-END
           MOVE    ZERO                TO  FLG-DCSRY
      *
           PERFORM UNTIL ( FLG-DCSRY       NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               READ    DCSRY-FILE     NEXT
               AT  END
                       MOVE    1           TO  FLG-DCSRY
               NOT AT  END
      *
                   IF    ( DCACCT-PTID     NOT =   DCSRY-PTID )
                    OR   ( DCACCT-ZAINUM   NOT =   DCSRY-ZAINUM )
                    OR   ( DCACCT-ACCTKBN  NOT =   DCSRY-ACCTKBN )
                       MOVE    1           TO  FLG-DCSRY
                   ELSE
                       MOVE    1           TO  FLG-LOOP-END
                   END-IF
      *
                   IF    ( DCSRY-SRYCD (1:1)   =   "1" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOP-END
                   END-IF
      *
               END-READ
               IF    ( STS-COMMON  >   10 )
                       MOVE    1           TO  FLG-DCSRY
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-DCSRY-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療会計START処理
      *****************************************************************
       901-DCACCT-START-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-DCACCT
      *
           INITIALIZE                      DCACCT-KEY
           MOVE    DTCHK003-PTID       TO  DCACCT-PTID
      *
           START   DCACCT-FILE
               KEY IS  >=   DCACCT-KEY
           INVALID
               MOVE    1               TO  FLG-DCACCT
           NOT INVALID
               PERFORM 901-DCACCT-READ-SEC
           END-START
      *
           .
       901-DCACCT-START-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療会計読込処理
      *****************************************************************
       901-DCACCT-READ-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-LOOP-END
           MOVE    ZERO                TO  FLG-DCACCT
      *
           PERFORM UNTIL ( FLG-DCACCT      NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               READ    DCACCT-FILE     NEXT
               AT  END
                       MOVE    1           TO  FLG-DCACCT
               NOT AT  END
      *
                   IF    ( KEY-PTID    NOT =   DCACCT-PTID )
                       MOVE    1           TO  FLG-DCACCT
                   ELSE
                       MOVE    1           TO  FLG-LOOP-END
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
       901-DCACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療行為START処理
      *****************************************************************
       901-DCSRY-START-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-DCSRY
      *
           INITIALIZE                      DCSRY-KEY
           MOVE    DCACCT-PTID         TO  DCSRY-PTID
           MOVE    DCACCT-ZAINUM       TO  DCSRY-ZAINUM
           MOVE    DCACCT-ACCTKBN      TO  DCSRY-ACCTKBN
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
       901-DCSRY-START-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療行為読込処理
      *****************************************************************
       901-DCSRY-READ-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-LOOP-END
           MOVE    ZERO                TO  FLG-DCSRY
      *
           PERFORM UNTIL ( FLG-DCSRY       NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               READ    DCSRY-FILE     NEXT
               AT  END
                       MOVE    1           TO  FLG-DCSRY
               NOT AT  END
      *
                   IF    ( DCACCT-PTID     NOT =   DCSRY-PTID )
                    OR   ( DCACCT-ZAINUM   NOT =   DCSRY-ZAINUM )
                    OR   ( DCACCT-ACCTKBN  NOT =   DCSRY-ACCTKBN )
                       MOVE    1           TO  FLG-DCSRY
                   ELSE
                       MOVE    1           TO  FLG-LOOP-END
                   END-IF
      *
               END-READ
               IF    ( STS-COMMON  >   10 )
                       MOVE    1           TO  FLG-DCSRY
               END-IF
      *
           END-PERFORM
      *
           .
      *
       901-DCSRY-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック診療行為読込処理
      *****************************************************************
       902-DCSRY-READ-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DCSRY
      *
           READ    DCSRY-FILE      NEXT
           AT  END
               MOVE    1           TO  FLG-DCSRY
           END-READ
           IF    ( STS-COMMON  >   10 )
               MOVE    1           TO  FLG-DCSRY
           END-IF
      *
           .
      *
       902-DCSRY-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル（電子点数表−算定回数）読込処理
      *****************************************************************
       900-DTCHK013-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK013
      *
           MOVE    DCSRY-PTID      TO  DTCHK013-PTID
           MOVE    DCSRY-SRYCD     TO  DTCHK013-SRYCD
           MOVE    MASTER-TANICD   TO  DTCHK013-TANICD
           MOVE    WRK-MSTKBN      TO  DTCHK013-MSTKBN
      *
           READ    DTCHK013-FILE
           INVALID
               MOVE    1       TO  FLG-DTCHK013
           END-READ
      *
           .
      *
       900-DTCHK013-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル（電子点数表−算定回数）読込処理
      *****************************************************************
       901-DTCHK013-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK013
           MOVE    ZERO            TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-DTCHK013    NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               READ    DTCHK013-FILE       NEXT
               AT  END
                   MOVE    1       TO  FLG-DTCHK013
               NOT AT  END
      *
                   MOVE    1       TO  FLG-LOOP-END
      *
                   IF    ( DTCHK013-NYUGAIKBN
                                       NOT =   ORCBSD01-NYUGAIKBN )
                       MOVE    ZERO    TO  FLG-LOOP-END
                   END-IF
      *
               END-READ
               IF    ( STS-COMMON  >   10 )
                   MOVE    1       TO  FLG-DTCHK013
               END-IF
      *
           END-PERFORM
      *
           .
      *
       901-DTCHK013-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY17-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    DTCHK003-PTID       TO  PTNYUINRRK-PTID
           MOVE    WRK-EOMYMD          TO  PTNYUINRRK-NYUINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-PTNYUINRRK-KEY17-FET-SEC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
      *
       900-PTNYUINRRK-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴FETCH処理
      *****************************************************************
       900-PTNYUINRRK-KEY17-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           MOVE    ZERO                TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               MOVE    "tbl_ptnyuinrrk"
                                       TO  MCP-TABLE
               MOVE    "key17"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
      *
                   MOVE    1           TO  FLG-LOOP-END
      *
                   IF    ( PTNYUINRRK-JTIKBN   =   "5" OR "6" )
                       MOVE    ZERO    TO  FLG-LOOP-END
                   END-IF
      *
               ELSE
                   INITIALIZE              PTNYUINRRK-REC
                   MOVE    1           TO  FLG-PTNYUINRRK
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-PTNYUINRRK-KEY17-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY60-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    DTCHK003-PTID       TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key60"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           .
      *
       900-PTNYUINRRK-KEY60-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴FETCH処理
      *****************************************************************
       900-PTNYUINRRK-KEY60-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key60"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           .
      *
       900-PTNYUINRRK-KEY60-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY38-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           COMPUTE PTNYUINRRK-RRKNUM = PTNYUINRRK-RRKNUM + 1
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           .
      *
       900-PTNYUINRRK-KEY38-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴FETCH処理
      *****************************************************************
       900-PTNYUINRRK-KEY38-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           .
      *
       900-PTNYUINRRK-KEY38-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHK-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-CHKMST
      *
           INITIALIZE                  CHK-REC
           MOVE    SPA-HOSPNUM     TO  CHK-HOSPNUM
           MOVE    SEL-CHKKBN      TO  CHK-CHKKBN
           MOVE    DCSRY-SRYCD     TO  CHK-SRYCD
           MOVE    SPA-SRYYMD      TO  CHK-YUKOSTYMD
                                       CHK-YUKOEDYMD
           MOVE    CHK-REC         TO  MCPDATA-REC
           MOVE    "tbl_chk"       TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK-REC
           ELSE
               MOVE    1           TO  FLG-CHKMST
               INITIALIZE              CHK-REC
           END-IF
      *
           .
       900-CHK-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタFETCH処理
      *****************************************************************
       900-CHK-KEY2-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-CHKMST
      *
           MOVE    "tbl_chk"       TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK-REC
           ELSE
               MOVE    1           TO  FLG-CHKMST
               INITIALIZE              CHK-REC
           END-IF
      *
           .
       900-CHK-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（算定回数）ファイルSTART処理
      *****************************************************************
       900-DTCHK014-START-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-DTCHK014
      *
           INITIALIZE                      DTCHK014-KEY
           MOVE    DCSRY-SRYCD         TO  DTCHK014-SRYCD
      *
           START   DTCHK014-FILE
               KEY IS  >=   DTCHK014-KEY
           INVALID
               PERFORM 9001-DTCHK014-START-SEC
           NOT INVALID
               PERFORM 900-DTCHK014-READ-SEC
               IF    ( FLG-DTCHK014    NOT =   ZERO )
                   PERFORM 9001-DTCHK014-START-SEC
               END-IF
           END-START
      *
           .
      *
       900-DTCHK014-START-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（算定回数）ファイルSTART処理
      *****************************************************************
       9001-DTCHK014-START-SEC          SECTION.
      *
           MOVE    ZERO            TO  CNT-ETENSU5
      *
           PERFORM 900-ETENSU5-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-ETENSU5 NOT =   ZERO )
      *
               COMPUTE CNT-ETENSU5 =   CNT-ETENSU5 +   1
      *
               MOVE    ETENSU5-REC TO  DTCHK014-REC
               WRITE   DTCHK014-REC
      *
               PERFORM 900-ETENSU5-KEY3-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_etensu_5"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( CNT-ETENSU5         >   ZERO )
               INITIALIZE                  DTCHK014-KEY
               MOVE    DCSRY-SRYCD     TO  DTCHK014-SRYCD
               START   DTCHK014-FILE
               KEY IS  >=   DTCHK014-KEY
               INVALID
                   MOVE    1           TO  FLG-DTCHK014
               NOT INVALID
                   PERFORM 900-DTCHK014-READ-SEC
               END-START
           ELSE
      *
      *        読み飛ばし用のダミーデータ作成
      *        （ＤＢ検索回避用）
               INITIALIZE                  DTCHK014-KEY
               MOVE    DCSRY-SRYCD     TO  DTCHK014-SRYCD
               MOVE    "00000000"      TO  DTCHK014-YUKOSTYMD
               MOVE    "99999999"      TO  DTCHK014-YUKOEDYMD
               MOVE    ZERO            TO  DTCHK014-TANICD
               WRITE   DTCHK014-REC
      *
               MOVE    1               TO  FLG-DTCHK014
      *
           END-IF
      *
           .
      *
       9001-DTCHK014-START-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（算定回数）ファイルREAD処理
      *****************************************************************
       900-DTCHK014-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK014
           MOVE    ZERO            TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-DTCHK014    NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               READ    DTCHK014-FILE       NEXT
               AT  END
                   MOVE    1       TO  FLG-DTCHK014
               NOT AT END
      *
                   MOVE    1       TO  FLG-LOOP-END
      *
                   IF    ( DTCHK014-SRYCD      NOT =   DCSRY-SRYCD )
                       MOVE    ZERO    TO  FLG-LOOP-END
                   END-IF
      *
                   IF    ( DTCHK014-TANICD     =   53 OR 121 OR 131
                                               OR 132 OR 133 OR 134
                                               OR 138 OR 142 OR 143
                                               OR 144 OR 145 OR 146
                                               OR 147 OR 148 )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-LOOP-END
                   END-IF
      *
               END-READ
               IF    ( STS-COMMON  >   10 )
                   MOVE    1       TO  FLG-DTCHK014
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-DTCHK014-READ-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（算定回数）検索処理
      *****************************************************************
       900-ETENSU5-KEY3-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-ETENSU5
      *
           INITIALIZE                      ETENSU5-REC
           MOVE    DCSRY-SRYCD         TO  ETENSU5-SRYCD
           MOVE    SPA-SRYYMD          TO  ETENSU5-YUKOSTYMD
                                           ETENSU5-YUKOEDYMD
           MOVE    ETENSU5-REC         TO  MCPDATA-REC
           MOVE    "tbl_etensu_5"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ETENSU5-REC
           ELSE
               MOVE    1               TO  FLG-ETENSU5
               INITIALIZE                  ETENSU5-REC
           END-IF
      *
           .
      *
       900-ETENSU5-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（算定回数）FETCH処理
      *****************************************************************
       900-ETENSU5-KEY3-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-ETENSU5
      *
           MOVE    "tbl_etensu_5"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ETENSU5-REC
           ELSE
               MOVE    1               TO  FLG-ETENSU5
               INITIALIZE                  ETENSU5-REC
           END-IF
      *
           .
      *
       900-ETENSU5-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（補助）検索処理
      *****************************************************************
       900-ETENSU1-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ETENSU1
      *
           INITIALIZE                      ETENSU1-REC
           MOVE    DCSRY-SRYCD         TO  ETENSU1-SRYCD
           MOVE    SPA-SRYYMD          TO  ETENSU1-YUKOSTYMD
                                           ETENSU1-YUKOEDYMD
           MOVE    ETENSU1-REC         TO  MCPDATA-REC
           MOVE    "tbl_etensu_1"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ETENSU1-REC
           ELSE
               MOVE    1               TO  FLG-ETENSU1
               INITIALIZE                  ETENSU1-REC
           END-IF
      *
           MOVE    "tbl_etensu_1"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-ETENSU1-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（包括）検索処理
      *****************************************************************
       900-ETENSU2-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-ETENSU2
      *
           INITIALIZE                      ETENSU2-REC
           MOVE    SEL-H-GROUP         TO  ETENSU2-H-GROUP
           MOVE    SPA-SRYYMD          TO  ETENSU2-YUKOSTYMD
                                           ETENSU2-YUKOEDYMD
           MOVE    ETENSU2-REC         TO  MCPDATA-REC
           MOVE    "tbl_etensu_2"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ETENSU2-REC
           ELSE
               MOVE    1               TO  FLG-ETENSU2
               INITIALIZE                  ETENSU2-REC
           END-IF
      *
           .
      *
       900-ETENSU2-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（包括）FETCH処理
      *****************************************************************
       900-ETENSU2-KEY2-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-ETENSU2
      *
           MOVE    "tbl_etensu_2"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ETENSU2-REC
           ELSE
               MOVE    1               TO  FLG-ETENSU2
               INITIALIZE                  ETENSU2-REC
           END-IF
      *
           .
      *
       900-ETENSU2-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（背反）検索処理
      *****************************************************************
       900-ETENSU3-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ETENSU3
      *
           INITIALIZE                      ETENSU3-REC
           MOVE    DCSRY-SRYCD         TO  ETENSU3-SRYCD1
           MOVE    SPA-SRYYMD          TO  ETENSU3-YUKOSTYMD
                                           ETENSU3-YUKOEDYMD
           MOVE    ETENSU3-REC         TO  MCPDATA-REC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-ETENSU3-FET-SEC
           ELSE
               MOVE    1               TO  FLG-ETENSU3
               INITIALIZE                  ETENSU3-REC
           END-IF
      *
           .
      *
       900-ETENSU3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    電子点数表（背反）FETCH処理
      *****************************************************************
       900-ETENSU3-FET-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ETENSU3
           MOVE    ZERO                TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-ETENSU3     NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               MOVE    WRK-TABLE           TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  ETENSU3-REC
      *
                   MOVE    1               TO  FLG-LOOP-END
      *
                   IF    ( SYS-1101-SRYHEIEXPKBN   =   "2" OR SPACE )
                       IF    ( ETENSU3-TOKUREI     NOT =   ZERO )
                           MOVE    ZERO    TO  FLG-LOOP-END
                       END-IF
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-ETENSU3
                   INITIALIZE                  ETENSU3-REC
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-ETENSU3-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       900-SRYACT-KEY20-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACT
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM     TO  SRY-HOSPNUM
           MOVE    "%"             TO  SRY-NYUGAIKBN
           MOVE    DTCHK013-PTID   TO  SRY-PTID
           MOVE    DTCHK013-SRYSTYMD (1:6)
                                   TO  SRY-SRYYM
           MOVE    WRK-LASTYM      TO  SRY-CREYMD
           MOVE    "%"             TO  SRY-SRYKBN
           MOVE    DTCHK013-SRYCD  TO  SRY-SRYCD (1)
                                       SRY-SRYCD (2)
                                       SRY-SRYCD (3)
                                       SRY-SRYCD (4)
                                       SRY-SRYCD (5)
           MOVE    SRYACT-REC      TO  MCPDATA-REC
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key20"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACT-REC
           ELSE
               MOVE    1           TO  FLG-SRYACT
               INITIALIZE              SRYACT-REC
           END-IF
      *
           .
       900-SRYACT-KEY20-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為FETCH処理
      *****************************************************************
       900-SRYACT-KEY20-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACT
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key20"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACT-REC
           ELSE
               MOVE    1           TO  FLG-SRYACT
               INITIALIZE              SRYACT-REC
           END-IF
      *
           .
       900-SRYACT-KEY20-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理
      *****************************************************************
       900-SRYACCT-KEY57-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACCT
      *
           INITIALIZE                  SRYACCT-REC
           MOVE    SRY-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    SRY-PTID        TO  ACCT-PTID
           MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key57"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACCT-REC
           ELSE
               MOVE    1           TO  FLG-SRYACCT
               INITIALIZE              SRYACCT-REC
           END-IF
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key57"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SRYACCT-KEY57-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為照会検索処理
      *****************************************************************
       900-NSRYSRH-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-NSRYSRH
      *
           INITIALIZE                  NSRYSRH-REC
           MOVE    SPA-HOSPNUM     TO  NSRYSRH-HOSPNUM
           MOVE    DTCHK013-PTID   TO  NSRYSRH-PTID
           MOVE    DTCHK013-SRYSTYMD (1:6)
                                   TO  NSRYSRH-SRYYM
           MOVE    WRK-LASTYM      TO  NSRYSRH-CREYMD
           MOVE    DTCHK013-SRYCD  TO  NSRYSRH-SRYCD
           MOVE    NSRYSRH-REC     TO  MCPDATA-REC
           MOVE    "tbl_nsrysrh"   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  NSRYSRH-REC
           ELSE
               MOVE    1           TO  FLG-NSRYSRH
               INITIALIZE              NSRYSRH-REC
           END-IF
      *
           .
       900-NSRYSRH-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為照会FETCH処理
      *****************************************************************
       900-NSRYSRH-KEY2-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-NSRYSRH
           MOVE    "tbl_nsrysrh"   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  NSRYSRH-REC
           ELSE
               MOVE    1           TO  FLG-NSRYSRH
               INITIALIZE              NSRYSRH-REC
           END-IF
      *
           .
       900-NSRYSRH-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴照会検索処理
      *****************************************************************
       900-NRRKSRH-KEY3-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-NRRKSRH
      *
           INITIALIZE                  NRRKSRH-REC
           MOVE    SPA-HOSPNUM     TO  NRRKSRH-HOSPNUM
           MOVE    NSRYSRH-PTID    TO  NRRKSRH-PTID
           MOVE    NSRYSRH-SRYYM   TO  NRRKSRH-SRYYMD
           MOVE    "__"            TO  NRRKSRH-SRYYMD (7:2)
           MOVE    NSRYSRH-ZAINUM  TO  NRRKSRH-ZAINUM
           MOVE    NRRKSRH-REC     TO  MCPDATA-REC
           MOVE    "tbl_nrrksrh"   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  NRRKSRH-REC
           ELSE
               MOVE    1           TO  FLG-NRRKSRH
               INITIALIZE              NRRKSRH-REC
           END-IF
      *
           .
       900-NRRKSRH-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴照会FETCH処理
      *****************************************************************
       900-NRRKSRH-KEY3-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-NRRKSRH
      *
           MOVE    "tbl_nrrksrh"   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  NRRKSRH-REC
           ELSE
               MOVE    1           TO  FLG-NRRKSRH
               INITIALIZE              NRRKSRH-REC
           END-IF
      *
           .
       900-NRRKSRH-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数付加検索処理
      *****************************************************************
       900-TENSUPLUS-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSUPLUS
      *
           PERFORM 900-DTCHK015-READ-SEC
           IF    ( FLG-DTCHK015    =   ZERO )
               IF    ( DTCHK015-OPID
                                   =   SPACE )
                   MOVE    DTCHK015-REC    TO  TENSUPLUS-REC
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1               TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    SPA-HOSPNUM     TO  TNSP-HOSPNUM
               MOVE    SEL-SRYCD       TO  TNSP-SRYCD
               MOVE    SPA-SRYYMD      TO  TNSP-YUKOSTYMD
                                           TNSP-YUKOEDYMD
               MOVE    TENSUPLUS-REC   TO  MCPDATA-REC
               MOVE    "tbl_tensuplus" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  TENSUPLUS-REC
                                           DTCHK015-REC
                   MOVE    "00000000"  TO  DTCHK015-YUKOSTYMD
                   MOVE    "99999999"  TO  DTCHK015-YUKOEDYMD
                   MOVE    SPACE       TO  DTCHK015-OPID
                   WRITE   DTCHK015-REC
               ELSE
                   MOVE    TENSUPLUS-REC
                                       TO  DTCHK015-REC
                   MOVE    "00000000"  TO  DTCHK015-YUKOSTYMD
                   MOVE    "99999999"  TO  DTCHK015-YUKOEDYMD
                   MOVE    "NONE"      TO  DTCHK015-OPID
                   WRITE   DTCHK015-REC
               END-IF
      *
               MOVE    "tbl_tensuplus" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           .
       900-TENSUPLUS-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数付加読込処理
      *****************************************************************
       900-DTCHK015-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK015
      *
           INITIALIZE                  DTCHK015-REC
           MOVE    SPA-HOSPNUM     TO  DTCHK015-HOSPNUM
           MOVE    SEL-SRYCD       TO  DTCHK015-SRYCD
           MOVE    "00000000"      TO  DTCHK015-YUKOSTYMD
           MOVE    "99999999"      TO  DTCHK015-YUKOEDYMD
           READ    DTCHK015-FILE
           INVALID
               MOVE    1           TO  FLG-DTCHK015
           END-READ
      *
           .
       900-DTCHK015-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           PERFORM 900-DTCHK016-READ-SEC
           IF    ( FLG-DTCHK016    =   ZERO )
               IF    ( DTCHK016-OPID
                                   =   SPACE )
                   MOVE    DTCHK016-TENSU-REC
                                           TO  TNS-TENSU-REC
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
               MOVE    SEL-SRYCD       TO  TNS-SRYCD
               MOVE    SPA-SRYYMD      TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
               MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  TNS-TENSU-REC
                                           DTCHK016-TENSU-REC
                   MOVE    "00000000"  TO  DTCHK016-YUKOSTYMD
                   MOVE    "99999999"  TO  DTCHK016-YUKOEDYMD
                   MOVE    SPACE       TO  DTCHK016-OPID
                   WRITE   DTCHK016-TENSU-REC
               ELSE
                   MOVE    TNS-TENSU-REC
                                       TO  DTCHK016-TENSU-REC
                   MOVE    "00000000"  TO  DTCHK016-YUKOSTYMD
                   MOVE    "99999999"  TO  DTCHK016-YUKOEDYMD
                   MOVE    "NONE"      TO  DTCHK016-OPID
                   WRITE   DTCHK016-TENSU-REC
                   INITIALIZE              TNS-TENSU-REC
                   MOVE    1           TO  FLG-TENSU
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
      *    点数読込処理
      *****************************************************************
       900-DTCHK016-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK016
      *
           INITIALIZE                  DTCHK016-TENSU-REC
           MOVE    SPA-HOSPNUM     TO  DTCHK016-HOSPNUM
           MOVE    SEL-SRYCD       TO  DTCHK016-SRYCD
           MOVE    "00000000"      TO  DTCHK016-YUKOSTYMD
           MOVE    "99999999"      TO  DTCHK016-YUKOEDYMD
           READ    DTCHK016-FILE
           INVALID
               MOVE    1           TO  FLG-DTCHK016
           END-READ
      *
           .
       900-DTCHK016-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ検索処理
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           PERFORM 900-DTCHK017-READ-SEC
           IF    ( FLG-DTCHK017        =   ZERO )
               IF    ( DTCHK016-OPID
                                   =   SPACE )
                   MOVE    DTCHK017-REC    TO  HKNCOMBI-REC
               ELSE
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
      *
               INITIALIZE                  HKNCOMBI-REC
               MOVE    SPA-HOSPNUM     TO  COMB-HOSPNUM
               MOVE    WRK-PTID        TO  COMB-PTID
               MOVE    WRK-HKNCOMBINUM TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
               MOVE    "tbl_hkncombi"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    HKNCOMBI-REC    TO  DTCHK017-REC
                   MOVE    SPACE           TO  DTCHK017-OPID
                   WRITE   DTCHK017-REC
               ELSE
                   MOVE    HKNCOMBI-REC    TO  DTCHK017-REC
                   MOVE    "NONE"          TO  DTCHK017-OPID
                   WRITE   DTCHK017-REC
                   MOVE    1               TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
      *
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    レセ記載検索処理
      *****************************************************************
       900-RECEKISAI-KEY5-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-RKISAI
      *
           INITIALIZE                  RKISAI-REC
           MOVE    DCSRY-SRYCD     TO  RKISAI-SRYCD
           MOVE    ORCBSD01-SRYYM  TO  RKISAI-STYM
                                       RKISAI-EDYM
           MOVE    RKISAI-REC      TO  MCPDATA-REC
           MOVE    "tbl_recekisai" TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-RECEKISAI-KEY5-FET-SEC
           ELSE
               MOVE    1           TO  FLG-RKISAI
               INITIALIZE              RKISAI-REC
           END-IF
      *
           .
       900-NRRKSRH-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴照会FETCH処理
      *****************************************************************
       900-RECEKISAI-KEY5-FET-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-RKISAI
           MOVE    ZERO            TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-RKISAI      NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               MOVE    "tbl_recekisai" TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  RKISAI-REC
      *
                   MOVE    1           TO  FLG-LOOP-END
      *
                   IF    ( RKISAI-JYOTAICD NOT =   SPACE )
                       MOVE    ZERO    TO  FLG-LOOP-END
                   END-IF
      *
                   IF    ( RKISAI-JYOKEN   =   1 OR 2 OR 3)
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-LOOP-END
                   END-IF
      *
               ELSE
                   MOVE    1           TO  FLG-RKISAI
                   INITIALIZE              RKISAI-REC
               END-IF
      *
           END-PERFORM
      *
           .
       900-RECEKISAI-KEY5-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ読込処理
      *****************************************************************
       900-DTCHK017-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK017
      *
           INITIALIZE                  DTCHK017-REC
           MOVE    SPA-HOSPNUM     TO  DTCHK017-HOSPNUM
           MOVE    WRK-PTID        TO  DTCHK017-PTID
           MOVE    WRK-HKNCOMBINUM TO  DTCHK017-HKNCOMBINUM
           READ    DTCHK017-FILE
           INVALID
               MOVE    1           TO  FLG-DTCHK017
           END-READ
      *
           .
       900-DTCHK017-READ-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ読込処理
      *****************************************************************
       900-DTCHK018-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK018
      *
           INITIALIZE                  DTCHK018-REC
           MOVE    SPA-HOSPNUM     TO  DTCHK018-HOSPNUM
           MOVE    SEL-CHKKBN      TO  DTCHK018-CHKKBN
           MOVE    DCSRY-SRYCD     TO  DTCHK018-SRYCD
           MOVE    "00000000"      TO  DTCHK018-YUKOSTYMD
           MOVE    "99999999"      TO  DTCHK018-YUKOEDYMD
           MOVE    SEL-CDKBN       TO  DTCHK018-CDKBN
           MOVE    SEL-CD          TO  DTCHK018-CD
           READ    DTCHK018-FILE
           INVALID
               MOVE    1           TO  FLG-DTCHK018
           END-READ
      *
           .
       900-DTCHK018-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル（診療行為コード）
      *****************************************************************
       900-DTCHK019-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK019
      *
           INITIALIZE                  DTCHK019-REC
           MOVE    SEL-SRYCD       TO  DTCHK019-SRYCD
           READ    DTCHK019-FILE
           INVALID
               MOVE    1           TO  FLG-DTCHK019
           END-READ
      *
           .
       900-DTCHK019-READ-EXT.
           EXIT.
      *****************************************************************
      *    データチェック中間ファイル（コメント関連）
      *****************************************************************
       900-DTCHK022-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-DTCHK022
      *
           INITIALIZE                  DTCHK022-REC
           MOVE    DCSRY-SRYCD     TO  DTCHK022-ISAM-SRYCD
           READ    DTCHK022-FILE
           INVALID
               MOVE    1           TO  FLG-DTCHK022
           END-READ
      *
           .
       900-DTCHK022-READ-EXT.
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
