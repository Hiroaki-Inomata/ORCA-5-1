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
       PROGRAM-ID.             ORCT0040.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 定期請求
      *  コンポーネント名  : 定期請求データ更新
      *  管理者            :
      *  作成日付    作業者        記述
      *  02/01/07    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL−太田   03.09.02  定期請求印刷情報に設定する
      *                                     氏名をカナ氏名に修正
      * 01.00.02     NACL-太田    03.11.12  更新日付編集対応
      * 01.00.03     NACL-太田    04.03.30  訂正更新処理追加
      *  03.05.00    NACL-太田    07/05/31  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *
      *    定期請求収納ファイル(IN) <-- 新規作成
           SELECT  TEIKI06-FILE        ASSIGN  TEIKI06PARA
                   ORGANIZATION        LINE SEQUENTIAL
                                       FILE    STATUS  IS  STS-TEIKI06.
      *
      *    請求取消対象収納ファイル(IN)
           SELECT  TEIKI07-FILE        ASSIGN   TEIKI07PARA
                   ORGANIZATION        LINE SEQUENTIAL
                                       FILE    STATUS  IS  STS-TEIKI07.
      *
      *    単独更新用日別収納ファイル(IN)
           SELECT  TEIKI08-FILE        ASSIGN   TEIKI08PARA
                   ORGANIZATION        LINE SEQUENTIAL
                                       FILE    STATUS  IS  STS-TEIKI08.
      *
      *    定期請求収納ファイル(IN) <-- 訂正更新
           SELECT  TEIKI09-FILE        ASSIGN  TEIKI09PARA
                   ORGANIZATION        LINE SEQUENTIAL
                                       FILE    STATUS  IS  STS-TEIKI09.
      *
      *    エラーファイル
           SELECT  TEIKIERR-FILE       ASSIGN  TEIKIERR
                                       FILE    STATUS  IS  STS-TEIKIERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    定期請求収納ファイル(OT) <-- 新規追加用
       FD  TEIKI06-FILE.
       01  TEIKI06-REC.
           COPY  "CPTEIKI01.INC"       REPLACING   //TEIKI01-//
                                       BY          //TEIKI06-//.
      *
      *    請求取消対象収納ファイル
       FD  TEIKI07-FILE.
       01  TEIKI07-REC.
           COPY    "CPSYUNOU.INC"      REPLACING   //SYU-//
                                       BY          //TEIKI07-//.
      *
      *    単独更新用日別収納ファイル
       FD  TEIKI08-FILE.
       01  TEIKI08-REC.
      *    更新区分（１：更新（追加）／２：削除）
         02    TEIKI08-UPD-FLG         PIC 9(01).
         02    TEIKI08-SYUDAY-DAT.
           COPY    "CPSYUDAY.INC"      REPLACING   //SDAY-//
                                       BY          //TEIKI08-SDAY-//.
      *
      *    定期請求収納ファイル(OT) <-- 訂正更新用
       FD  TEIKI09-FILE.
       01  TEIKI09-REC.
           COPY  "CPTEIKI01.INC"       REPLACING   //TEIKI01-//
                                       BY          //TEIKI09-//.
      *
      *    エラーファイル
       FD  TEIKIERR-FILE.
       01  TEIKIERR-REC.
           03  TEIKIERR-ERRCD          PIC X(04).
           03  TEIKIERR-WARNING        PIC 9(01).
           03  TEIKIERR-ERRMSG         PIC X(195).
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    定期請求収納ファイル名称領域 
           COPY    "CPCOMMONDAT3.INC"  REPLACING  //TMPFLPARA//
                                       BY         //TEIKI06PARA//.
      *
      *    請求取消対象収納ファイル名称領域 
           COPY    "CPCOMMONDAT3.INC"  REPLACING  //TMPFLPARA//
                                       BY         //TEIKI07PARA//.
      *
      *    単独更新用日別収納ファイル(IN)
           COPY    "CPCOMMONDAT3.INC"  REPLACING  //TMPFLPARA//
                                       BY         //TEIKI08PARA//.
      *
      *    定期請求収納ファイル(OT) <-- 訂正更新用
           COPY    "CPCOMMONDAT3.INC"  REPLACING  //TMPFLPARA//
                                       BY         //TEIKI09PARA//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TEIKIERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TEIKIERR            PIC X(02).
           03  STS-TEIKI06             PIC X(02).
           03  STS-TEIKI07             PIC X(02).
           03  STS-TEIKI08             PIC X(02).
           03  STS-TEIKI09             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-WARNING             PIC 9(01).
           03  FLG-TEIKI06             PIC 9(01).
           03  FLG-TEIKI07             PIC 9(01).
           03  FLG-TEIKI08             PIC 9(01).
           03  FLG-TEIKI09             PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-SYUMEI              PIC 9(01).
           03  FLG-SYUDAY              PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SYS5012             PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-TEIKIPRT            PIC 9(01).
           03  FLG-PTTEIKIRRK          PIC 9(01).
           03  FLG-TEIKIKANRI          PIC 9(01).
           03  FLG-TEIKIRRK            PIC 9(01).
           03  FLG-HIT                 PIC 9(01).
           03  FLG-DENPNUM-ERR         PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-BUNBEN              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-TEIKI06             PIC 9(06).
           03  CNT-TEIKI08             PIC 9(06).
           03  CNT-TEIKI09             PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX1                    PIC 9(04). 
           03  IDX2                    PIC 9(04). 
           03  IDX3                    PIC 9(04). 
           03  IDX4                    PIC 9(04). 
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SKYMONEY            PIC S9(07).
           03  WRK-NYUKIN-TOTAL        PIC S9(07).
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-DENPNUM-X           PIC X(08).
           03  WRK-STR                 PIC X(200).
           03  WRK-LEN                 PIC 9(05).
           03  WRK-SHO                 PIC 9(05).
           03  WRK-AMARI               PIC 9(05).
           03  WRK-CREYMD              PIC X(08).
           03  WRK-DENPEDANUM          PIC 9(02).
           03  WRK-NYUKINRENNUM        PIC 9(02).
           03  WRK-KUGIRI              PIC X(02).
           03  WRK-PTID                PIC 9(10).
           03  WRK-NYUKIN-HOHO         PIC X(02).
           03  WRK-NYUKIN-BUNRUI       PIC X(02).
           03  WRK-NYUKIN-JYOTAI       PIC X(01).
           03  WRK-NYUKIN-KBN          PIC X(01).
           03  WRK-KBT-G.
               05  WRK-KBT-9           PIC 9(01).
               05  WRK-KBT-R   REDEFINES   WRK-KBT-9.
                   07  WRK-KBT         PIC X(01).
           03  WRK-YMD.
               05  WRK-YY                  PIC X(04).
               05  WRK-MM                  PIC X(02).
               05  WRK-DD                  PIC X(02).
           03  WRK-EDTYMD-G.
               05  WRK-EDTYMD1             PIC X(10).
               05  WRK-EDTYMD3             PIC X(22).
           03  WRK-SHELLMSG-G.
               05  WRK-KBTMSG              PIC X(06).
               05  WRK-MONMSG              PIC X(06).
               05  WRK-SHELLMSG            PIC X(100).
           03  WRK-BASE-MONEY              PIC S9(08).
      *
       01  SRH-AREA.
           03  SRH-IDX                     PIC 9(05).
           03  SRH-1041-DAT.
               05  SRH-1041-MAX            PIC 9(05).
               05  SRH-1041-OCC            OCCURS  99.
                   07  SRH-1041-KBNCDL     PIC X(02).
                   07  SRH-1041-BUNRUIL    PIC X(02).
                   07  SRH-1041-JYOTAIL    PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-H270101               PIC X(08)   VALUE "20150101".
      *
       01  ERR-EDIT-AREA.
           03  ERR-WARNING             PIC 9(01).
           03  ERR-PTID                PIC 9(10).
           03  ERR-PTNUM               PIC X(20).
           03  ERR-ERRCD               PIC X(80).
           03  ERR-ERRMSG              PIC X(80).
           03  ERR-HEN-ERRMSG          PIC X(80).
      *
       01  ERR-MSG-AREA.
           03  ERR-MSG01                   PIC X(100)    VALUE
               "定期請求管理情報が取得できませんでした".
           03  ERR-MSG02                   PIC X(100)    VALUE
               "定期請求管理情報の削除に失敗しました".
           03  ERR-MSG03                   PIC X(100)    VALUE
               "定期請求印刷情報情報の削除に失敗しました".
           03  ERR-MSG04                   PIC X(100)    VALUE
               "仮収納データ削除処理に失敗しました".
           03  ERR-MSG05                   PIC X(100)    VALUE
               "患者定期請求履歴情報の削除に失敗しました".
           03  ERR-MSG06                   PIC X(100)    VALUE
               "患者定期請求履歴情報の削除に失敗しました".
           03  ERR-MSG07                   PIC X(100)    VALUE
               "収納明細情報の更新に失敗しました".
           03  ERR-MSG08                   PIC X(100)    VALUE
               "収納情報の更新に失敗しました".
           03  ERR-MSG09                   PIC X(100)    VALUE
               "伝票番号の更新に失敗しました".
           03  ERR-MSG10                   PIC X(100)    VALUE
               "日別収納マスタの更新に失敗しました".
           03  ERR-MSG11                   PIC X(100)    VALUE
               "日別収納マスタの削除に失敗しました".
           03  ERR-MSG12                   PIC X(100)    VALUE
               "患者定期請求履歴情報の更新に失敗しました".
           03  ERR-MSG13                   PIC X(100)    VALUE
               "患者情報の取得に失敗しました".
           03  ERR-MSG14                   PIC X(100)    VALUE
               "最新入院履歴の取得に失敗しました".
           03  ERR-MSG15                   PIC X(100)    VALUE
               "患者番号の取得に失敗しました".
           03  ERR-MSG16                   PIC X(100)    VALUE
               "定期請求印刷情報の作成に失敗しました".
           03  ERR-MSG17                   PIC X(100)    VALUE
               "定期請求管理情報の作成に失敗しました".
           03  ERR-MSG18                   PIC X(100)    VALUE
               "定期請求履歴情報の更新に失敗しました".
           03  ERR-MSG19                   PIC X(100)    VALUE
               "定期請求履歴情報の作成に失敗しました".
           03  ERR-MSG20                   PIC X(100)    VALUE
               "処理対象のデータがありませんでした".
           03  ERR-MSG21                   PIC X(100)    VALUE
               "収納明細情報の更新に失敗しました".
           03  ERR-MSG22                   PIC X(100)    VALUE
               "収納明細情報の更新に失敗しました".
           03  ERR-MSG23                   PIC X(100)    VALUE
               "収納明細情報の更新に失敗しました".
           03  ERR-MSG24                   PIC X(100)    VALUE
               "収納明細情報の更新に失敗しました".
           03  ERR-MSG25                   PIC X(100)    VALUE
               "収納明細情報の更新に失敗しました".
           03  ERR-MSG26                   PIC X(100)    VALUE
               "収納明細情報の更新に失敗しました".
           03  ERR-MSG27                   PIC X(100)    VALUE
               "収納情報の更新に失敗しました".
           03  ERR-MSG28                   PIC X(100)    VALUE
               "収納情報の更新に失敗しました".
           03  ERR-MSG29                   PIC X(100)    VALUE
               "収納情報の更新に失敗しました".
           03  ERR-MSG30                   PIC X(100)    VALUE
               "収納情報の更新に失敗しました".
           03  ERR-MSG31                   PIC X(100)    VALUE
               "日別収納マスタの更新に失敗しました".
           03  ERR-MSG32                   PIC X(100)    VALUE
               "患者定期請求履歴情報の更新に失敗しました".
           03  ERR-MSG33                   PIC X(100)    VALUE
               "患者定期請求履歴情報の更新に失敗しました".
           03  ERR-MSG34                   PIC X(100)    VALUE
               "患者定期請求履歴情報の更新に失敗しました".
           03  ERR-MSG35                   PIC X(100)    VALUE
               "患者定期請求履歴情報の更新に失敗しました".
           03  ERR-MSG36                   PIC X(100)    VALUE
               "患者情報の取得に失敗しました".
           03  ERR-MSG37                   PIC X(100)    VALUE
               "定期請求印刷情報の作成に失敗しました".
           03  ERR-MSG38                   PIC X(100)    VALUE
               "定期請求管理情報の作成に失敗しました".
           03  ERR-MSG39                   PIC X(100)    VALUE
               "定期請求管理情報の更新に失敗しました".
           03  ERR-MSG40                   PIC X(100)    VALUE
               "収納情報の更新に失敗しました".
           03  ERR-MSG41                   PIC X(100)    VALUE
               "収納情報の更新に失敗しました".
           03  ERR-MSG42                   PIC X(100)    VALUE
               "収納情報の更新に失敗しました".
           03  ERR-MSG43                   PIC X(100)    VALUE
               "収納情報の更新に失敗しました".
           03  ERR-MSG44                   PIC X(100)    VALUE
               "分娩管理情報の更新に失敗しました".
           03  ERR-MSG45                   PIC X(100)    VALUE
               "分娩管理情報の更新に失敗しました".
           03  ERR-MSG88                   PIC X(100)    VALUE
               "キャンセルしました".
      *
      *    定期請求管理集計用ワーク
       01  SUMTEIKIKANRI-REC.
         02    SUMTEIKIKANRI-MAX       PIC 9(05).
         02    SUMTEIKIKANRI-OCC       OCCURS 50.
           COPY  "CPTEIKIKANRI.INC"    REPLACING  //TEIKIKANRI-//
                                       BY         //SUMTEIKIKANRI-//.
      *
      *    日別収納退避ワーク
       01  WKSYUDAY-REC.
           COPY  "CPSYUDAY.INC"        REPLACING  //SDAY-//
                                       BY         //WKSDAY-//.
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    定期請求一括処理パラメータ
           COPY    "CPTEIKI1PARA.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    日別収納
       01  SYUDAY-REC.
           COPY    "CPSYUDAY.INC".
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    定期請求管理
       01  TEIKIKANRI-REC.
           COPY    "CPTEIKIKANRI.INC".
      *
      *    定期請求印刷
       01  TEIKIPRT-REC.
           COPY    "CPTEIKIPRT.INC".
      *
      *    定期請求履歴
       01  TEIKIRRK-REC.
           COPY    "CPTEIKIRRK.INC".
      *
      *    患者定期請求履歴
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *    分娩管理
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
      *    伝票番号
           COPY    "CPSK0042.INC".
      *    入金方法
           COPY  "CPSK1041.INC".
      *    定期請求管理情報
           COPY  "CPSK5010.INC".
      *    定期請求作成指示（個別指示）コード
           COPY  "CPSK5012.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    分娩負担金サブ
       01  LNKBUNBEN-REC.
           COPY  "CPBUNBEN.INC"    REPLACING  //BUNBEN-//
                                           BY //LNKBUNBEN-//.
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    定期請求サブ
           COPY    "CPORCSTEIKI01.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    日報キー取得サブ
           COPY "CPORCSDAILYKEY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    収納更新差額取得
           COPY  "CPORCSS002.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "CPCOMMAND-PARAM.INC".
      *
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
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-TERM-SEC
      *
           PERFORM 990-PROGRAM-FINISH-SEC
      *
           .
       000-PROC-EXT.
           EXIT.
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       INDEX-AREA
                                       CNT-AREA
                                       WRK-AREA
                                       SPA-AREA
                                       SUMTEIKIKANRI-REC
                                       TEIKI1PARA-AREA
                                       ERR-EDIT-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    TEIKI1P-ARG1
                                               TEIKI1P-SYOKBN
                                               TEIKI1P-KIKAN-OCC (1)
                                               TEIKI1P-KIKAN-OCC (2)
                                               TEIKI1P-KIKAN-OCC (3)
                                               TEIKI1P-DENPPRTYMD
                                               TEIKI1P-JOBID
                                               TEIKI1P-SHELLID
                                               TEIKI1P-HOSPNUM
                                               TEIKI1P-ERRFILE
                                               TEIKI1P-KBT
                                               TEIKI1P-JOBYMD
                                               TEIKI1P-JOBHMS
           END-UNSTRING
      *
           MOVE    TEIKI1P-HOSPNUM     TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
           PERFORM 1001-EDIT-SHELLMSG-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    TEIKI1P-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    TEIKI1P-JOBID       TO  JOB-JOBID
           MOVE    TEIKI1P-SHELLID     TO  JOB-SHELLID
           MOVE    "ORCT0040"          TO  JOB-PGID
           MOVE    WRK-SHELLMSG        TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    ZERO                TO  FLG-END
                                           FLG-ERR
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKI06PARA-HOSPNUM
           MOVE    "TEIKI006"          TO  TEIKI06PARA-FILE-ID
           MOVE    TEIKI1P-TERMID      TO  TEIKI06PARA-TERMID
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKI07PARA-HOSPNUM
           MOVE    "TEIKI007"          TO  TEIKI07PARA-FILE-ID
           MOVE    TEIKI1P-TERMID      TO  TEIKI07PARA-TERMID
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKI08PARA-HOSPNUM
           MOVE    "TEIKI008"          TO  TEIKI08PARA-FILE-ID
           MOVE    TEIKI1P-TERMID      TO  TEIKI08PARA-TERMID
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKI09PARA-HOSPNUM
           MOVE    "TEIKI009"          TO  TEIKI09PARA-FILE-ID
           MOVE    TEIKI1P-TERMID      TO  TEIKI09PARA-TERMID
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TEIKI06PARA-BASENAME    TO  SGETTEMP-BASENAMES (1)
           MOVE    TEIKI07PARA-BASENAME    TO  SGETTEMP-BASENAMES (2)
           MOVE    TEIKI08PARA-BASENAME    TO  SGETTEMP-BASENAMES (3)
           MOVE    TEIKI09PARA-BASENAME    TO  SGETTEMP-BASENAMES (4)
           MOVE    TEIKI1P-ERRFILE         TO  SGETTEMP-BASENAMES (5)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)  TO  TEIKI06PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)  TO  TEIKI07PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (3)  TO  TEIKI08PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (4)  TO  TEIKI09PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (5)  TO  TEIKIERR-FULLNAME
      *
           INITIALIZE                  ORCSTEIKI01AREA
           MOVE    TEIKI1P-JOBID   TO  STEIKI01-JOBID
           MOVE    TEIKI1P-SHELLID TO  STEIKI01-SHELLID
           CALL    "ORCSTEIKI01"   USING   ORCSTEIKI01AREA
                                           SPA-AREA
           IF    ( STEIKI01-STOP   =   1 )
               MOVE    "8888"      TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           OPEN    INPUT               TEIKI06-FILE
           OPEN    INPUT               TEIKI07-FILE
           OPEN    INPUT               TEIKI08-FILE
           OPEN    INPUT               TEIKI09-FILE
      *
      *    定期請求管理情報
           INITIALIZE                  SYS-5010-REC
           MOVE    "5010"              TO  SYS-5010-KANRICD
           MOVE    "*"                 TO  SYS-5010-KBNCD
           MOVE    TEIKI1P-SYSYMD      TO  SYS-5010-STYUKYMD
                                           SYS-5010-EDYUKYMD
           MOVE    SYS-5010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-5010-REC
           ELSE
               MOVE    "0001"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           PERFORM 1001-SRH-1041-HEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    シェルメッセージ編集処理
      *****************************************************************
       1001-EDIT-SHELLMSG-SEC          SECTION.
      *
           INITIALIZE                  WRK-SHELLMSG-G
      *
           IF    ( TEIKI1P-SYOKBN      =   "1" )
               MOVE    TEIKI1P-SRYYM   TO  WRK-YMD
               MOVE    "01"            TO  WRK-YMD (7:2)
               PERFORM 800-WAREKI-HEN-SEC
               MOVE    WRK-EDTYMD3 (11:6)  TO  WRK-MONMSG
           END-IF
      *
           EVALUATE    TEIKI1P-KBT
           WHEN    1
               MOVE    "１期："        TO  WRK-KBTMSG
           WHEN    2
               MOVE    "２期："        TO  WRK-KBTMSG
           WHEN    3
               MOVE    "３期："        TO  WRK-KBTMSG
           END-EVALUATE
      *
           STRING  WRK-MONMSG              DELIMITED   BY  SPACE
                   WRK-KBTMSG              DELIMITED   BY  SIZE
                   "定期請求データ更新"    DELIMITED   BY  SIZE
           INTO    WRK-SHELLMSG
           END-STRING
      *
           .
       1001-EDIT-SHELLMSG-EXT.
           EXIT.
      *****************************************************************
      *    入金方法
      *****************************************************************
       1001-SRH-1041-HEN-SEC               SECTION.
      *
      *    入金方法
           MOVE    ZERO            TO  SRH-1041-MAX
      *
           INITIALIZE                  SYS-1041-REC
           MOVE    "1041"          TO  SYS-1041-KANRICD
           MOVE    TEIKI1P-SYSYMD  TO  SYS-1041-STYUKYMD
                                       SYS-1041-EDYUKYMD
           MOVE    SYS-1041-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC TO  SYS-1041-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SRH-1041-MAX    >=  99 )
      *
               COMPUTE SRH-1041-MAX    =   SRH-1041-MAX    +   1
      *
               MOVE    SYS-1041-KBNCD  TO  SRH-1041-KBNCDL
                                                        (SRH-1041-MAX)
               MOVE    SYS-1041-NYKN-BUNRUI
                                       TO  SRH-1041-BUNRUIL
                                                        (SRH-1041-MAX)
      *
               IF    ( SYS-1041-NYKN-JYOTAI-T  =   SPACE )
                   MOVE    "3"     TO  SRH-1041-JYOTAIL (SRH-1041-MAX)
               ELSE
                   MOVE    SYS-1041-NYKN-JYOTAI-T
                                   TO  SRH-1041-JYOTAIL (SRH-1041-MAX)
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               IF    ( FLG-SYSKANRI        =   ZERO )
                   MOVE    SYSKANRI-REC TO  SYS-1041-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-SRH-1041-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    定期請求管理チェック処理
           PERFORM 2001-TEIKIKANRI-CHK-SEC
      *
      *    前回定期請求情報更新処理
           PERFORM 2002-MAE-SEIKYU-KOUSIN-SEC
      *
      *    定期請求情報更新処理（新規追加）
           PERFORM 2003-SEIKYU-KOUSIN-SEC
      *
      *    定期請求情報更新処理（訂正更新）
           PERFORM 2007-SEIKYU-KOUSIN-SEC
      *
      *    単独更新用日別収納更新処理
           PERFORM 2004-TEIKI08-KOUSIN-SEC
      *
      *    定期請求管理情報更新処理
           PERFORM 2005-TEIKIKANRI-KOUSIN-SEC
      *
           IF    ( TEIKI1P-SYOKBN  =   "1" )
      *        定期請求履歴情報更新処理
               PERFORM 2006-TEIKIRRK-KOUSIN-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    定期請求管理チェック処理
      *****************************************************************
       2001-TEIKIKANRI-CHK-SEC         SECTION.
      *
           PERFORM 900-TEIKIKANRI-KEY2-SEL-SEC
           IF    ( FLG-TEIKIKANRI  NOT =   ZERO )
      *
      *        定期請求管理情報削除処理
               PERFORM 900-TEIKIKANRI-DEL-SEC
      *
      *        定期請求印刷情報削除処理
               PERFORM 900-TEIKIPRT-DEL-SEC
           END-IF

      *
           .
       2001-TEIKIKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    前回定期請求情報更新処理
      *****************************************************************
       2002-MAE-SEIKYU-KOUSIN-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-TEIKI07
      *
      *    請求取消対象収納ファイル読込
           PERFORM 910-TEIKI07-READ-SEC
      *
           PERFORM UNTIL ( FLG-TEIKI07 NOT =   ZERO )
      *
      *        患者定期請求履歴データ更新処理
               PERFORM 20021-PTTEIKIRRK-KOUSIN-SEC
      *
      *        収納データ更新処理
               PERFORM 20021-SYUNOU-KOUSIN-SEC
      *
      *        請求取消対象収納ファイル読込
               PERFORM 910-TEIKI07-READ-SEC
      *
           END-PERFORM
      *
           .
       2002-MAE-SEIKYU-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    収納データ更新処理
      *****************************************************************
       20021-SYUNOU-KOUSIN-SEC         SECTION.
      *
           MOVE    TEIKI07-REC         TO  SYUNOU-REC
      *
           COMPUTE WRK-SKYMONEY    =   SYU-SKYMONEY    *   -1
      *
      *    請求取消として収納マスターを更新
           MOVE    ZERO                TO  SYU-SKYMONEY
      *                                    SYU-NYUKIN-TOTAL
           MOVE    "3"                 TO  SYU-DENPJTIKBN
      *
      *    収納明細作成処理
           PERFORM 20021-SYUMEI-SAKUSEI-SEC
      *
           MOVE    TEIKI1P-JOBYMD      TO  SYU-UPYMD
           MOVE    TEIKI1P-JOBHMS      TO  SYU-UPHMS
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    SYU-PTID        TO  ERR-PTID
               MOVE    "0008"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20021-SYUNOU-KOUSIN-EXT.
           EXIT. 
      *****************************************************************
      *    収納明細作成処理
      *****************************************************************
       20021-SYUMEI-SAKUSEI-SEC        SECTION.
      *
           PERFORM 800-SS002-SEC
      *
           PERFORM 900-SYUMEI-KEY7-SEL-SEC
      *
           COMPUTE WRK-DENPEDANUM      =   SMEI-DENPEDANUM +   1
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *    伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *    伝票番号枝番
           MOVE    WRK-DENPEDANUM      TO  SMEI-DENPEDANUM
      *    入金連番
           MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *    診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *    伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *    請求発行日（発行日の西暦）
           MOVE    SPACE               TO  SMEI-SKYPRINTYMD
      *    請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *    領収書発行日
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *    領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *    請求金額
           MOVE    WRK-SKYMONEY        TO  SMEI-SKYMONEY
      *
      *    入返金区分
           MOVE    SPACE               TO  SMEI-NYUHEN-KBN
      *    入返金額
           MOVE    ZERO                TO  SMEI-NYUHEN-MONEY
      *    入返金日
           MOVE    TEIKI1P-SYSYMD      TO  SMEI-NYUHEN-YMD
      *    入金方法
           MOVE    "00"                TO  SMEI-NYUKIN-HOHO
      *    状態区分
           MOVE    "5"                 TO  SMEI-JOUTAIKBN
      *
      *    収納明細存在チェック（すでに存在する場合はエラー）
           PERFORM 400-SYUMEI-CHK-SEC
           IF    ( FLG-SYUMEI          =   ZERO )
               MOVE    SMEI-PTID       TO  ERR-PTID
               MOVE    "0007"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    収納明細追加
           MOVE    TEIKI1P-JOBYMD      TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    TEIKI1P-JOBHMS      TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           PERFORM 800-SET-SS002-SMEI-SEC
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    SMEI-PTID       TO  ERR-PTID
               MOVE    "0021"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20021-SYUMEI-SAKUSEI-EXT.
           EXIT. 
      *****************************************************************
      *    患者定期請求履歴データ更新処理
      *****************************************************************
       20021-PTTEIKIRRK-KOUSIN-SEC     SECTION.
      *
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    TEIKI07-HOSPNUM      TO  PTTEIKIRRK-HOSPNUM
           MOVE    TEIKI07-PTID        TO  PTTEIKIRRK-PTID
           MOVE    TEIKI07-DENPNUM     TO  PTTEIKIRRK-DENPNUM
      *
           MOVE    TEIKI1P-JOBYMD      TO  PTTEIKIRRK-UPYMD
           MOVE    TEIKI1P-JOBHMS      TO  PTTEIKIRRK-UPHMS
      *
      *    患者定期請求履歴を更新
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "upd4"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF  (   MCP-RC          NOT =   ZERO )
               MOVE    PTTEIKIRRK-PTID TO  ERR-PTID
               MOVE    "0006"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20021-PTTEIKIRRK-KOUSIN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    定期請求情報更新処理（新規追加）
      *****************************************************************
       2003-SEIKYU-KOUSIN-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TEIKI06
      *
      *    定期請求収納ファイル読込
           PERFORM 910-TEIKI06-READ-SEC
      *
           PERFORM UNTIL ( FLG-TEIKI06 NOT =   ZERO )
      *
      *        伝票番号取得処理
               PERFORM 20031-DENPNUM-GET-SEC
      *
               MOVE    TEIKI06-SYU-DAT TO  SYUNOU-REC
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "1001"          TO  SYS-KANRICD
               MOVE    "*"             TO  SYS-KBNCD
               MOVE    SYU-SKYSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1001-REC
               IF    ( SYS-1001-SANKANUM   IS  NOT NUMERIC )
                OR   ( SYS-1001-SANKANUM   =   ZERO    )
                   MOVE    ZERO            TO  SYS-1001-SANKANUM
               END-IF
      *
      *        患者情報読み込み
               INITIALIZE              PTINF-REC
               MOVE    SYU-HOSPNUM      TO  PTINF-HOSPNUM
               MOVE    SYU-PTID        TO  PTINF-PTID
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( FLG-PTINF       NOT =   ZERO )
                   MOVE    PTINF-PTID  TO  ERR-PTID
                   MOVE    "0013"      TO  ERR-ERRCD
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
               PERFORM 200311-NYUKIN-HOHO-HEN-SEC
      *
               MOVE    WRK-DENPNUM     TO  SYU-DENPNUM
      *
      *        更新前チェック（既に重複するデータがある場合はエラー）
               PERFORM 400-SYUNOU-CHK-SEC
               IF    ( FLG-SYUNOU      =   ZERO )
                   MOVE    SYU-PTID    TO  ERR-PTID
                   MOVE    "0027"      TO  ERR-ERRCD
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
               MOVE    TEIKI1P-JOBYMD  TO  SYU-CREYMD
                                           SYU-UPYMD
               MOVE    TEIKI1P-JOBHMS  TO  SYU-UPHMS
      *
               PERFORM 800-SS002-SEC
      *
               MOVE    SYUNOU-REC      TO  MCPDATA-REC
               MOVE    "tbl_syunou"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    SYU-PTID    TO  ERR-PTID
                   MOVE    "0028"      TO  ERR-ERRCD
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
      *        収納明細作成処理
               PERFORM 20031-SYUMEI-SAKUSEI-SEC
      *
      *        日別収納作成処理
               IF    ( TEIKI06-SYUDAY-EXIST-FLG    =   1 )
                   PERFORM 20031-SYUDAY-SAKUSEI-SEC
               ELSE
                   PERFORM 20031-SYUDAY-DELETE-SEC
               END-IF
      *
      *        患者定期請求履歴更新処理
               PERFORM 20031-PTTEIKIRRK-KOUSIN-SEC
      *
      *        最新病棟番号別集計処理
               PERFORM 20031-NOW-BTUNUM-SUM-SEC
      *
      *        分娩情報更新処理
               PERFORM 20081-BUNBEN-KOUSIN-SEC
      *
      *        定期請求印刷情報作成処理
               PERFORM 20031-TEIKIPRT-SAKUSEI-SEC
      *
      *        定期請求収納ファイル読込
               PERFORM 910-TEIKI06-READ-SEC
      *
           END-PERFORM
      *
           .
       2003-SEIKYU-KOUSIN-EXT.
           EXIT. 
      *****************************************************************
      *    伝票番号取得処理
      *****************************************************************
       20031-DENPNUM-GET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
                                           WRK-DENPNUM
                                           FLG-DENPNUM-ERR
      *
           INITIALIZE                  SYS-0042-REC
           MOVE    "0042"              TO  SYS-0042-KANRICD
           MOVE    "*"                 TO  SYS-0042-KBNCD
           MOVE    TEIKI1P-SYSYMD      TO  SYS-0042-STYUKYMD
                                           SYS-0042-EDYUKYMD
           MOVE    SYS-0042-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-0042-REC
               MOVE    SYS-0042-DENPNUMMAX-NYU
                                       TO  WRK-DENPNUM-X
               IF    ( WRK-DENPNUM-X   =   SPACE )
                   MOVE    ZERO        TO  SYS-0042-DENPNUMMAX-NYU
               ELSE
                   MOVE    WRK-DENPNUM-X
                                       TO  SYS-0042-DENPNUMMAX-NYU
               END-IF
      *
               COMPUTE SYS-0042-DENPNUMMAX-NYU
                   =   SYS-0042-DENPNUMMAX-NYU +   1
      *
               MOVE    SYS-0042-DENPNUMMAX-NYU
                                       TO  WRK-DENPNUM
      *
               MOVE    TEIKI1P-JOBYMD  TO  SYS-0042-UPYMD
               MOVE    TEIKI1P-JOBHMS  TO  SYS-0042-UPHMS
      *
               MOVE    SYS-0042-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    1           TO  FLG-DENPNUM-ERR
               END-IF
           ELSE
                   MOVE    1           TO  FLG-DENPNUM-ERR
           END-IF
      *
           IF    ( FLG-DENPNUM-ERR NOT =   ZERO )
               MOVE    "0009"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20031-DENPNUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴データ更新処理
      *****************************************************************
       20031-PTTEIKIRRK-KOUSIN-SEC     SECTION.
      *
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    SYU-HOSPNUM          TO  PTTEIKIRRK-HOSPNUM
           MOVE    SYU-PTID            TO  PTTEIKIRRK-PTID
           MOVE    SYU-SRYYMD (1:6)    TO  PTTEIKIRRK-SRYYM
           MOVE    TEIKI06-SAKKBN      TO  PTTEIKIRRK-SAKKBN
           MOVE    SYU-HKNCOMBINUM     TO  PTTEIKIRRK-HKNCOMBINUM
           MOVE    ZERO                TO  PTTEIKIRRK-DELFLG
           MOVE    TEIKI1P-SYOKBN      TO  PTTEIKIRRK-SYOKBN
           MOVE    TEIKI06-KBT         TO  PTTEIKIRRK-KBT
           MOVE    TEIKI06-SKYSTYMD    TO  PTTEIKIRRK-SKYSTYMD
           MOVE    TEIKI06-SKYEDYMD    TO  PTTEIKIRRK-SKYEDYMD
           MOVE    SYU-TOTAL-HKNTEN    TO  PTTEIKIRRK-SKYTEN
           MOVE    SYU-SKYMONEY-TAX-SAI
                                       TO  PTTEIKIRRK-SKYMONEY-TAX-SAI
           MOVE    SYU-SKYMONEY        TO  PTTEIKIRRK-SKYMONEY
           MOVE    SYU-NYUIN-RRKNUM    TO  PTTEIKIRRK-RRKNUM
           MOVE    SYU-DENPNUM         TO  PTTEIKIRRK-DENPNUM
           MOVE    1                   TO  PTTEIKIRRK-DENPEDANUM
           MOVE    ZERO                TO  PTTEIKIRRK-SKYUPDFLG
      *
           MOVE    TEIKI1P-JOBYMD      TO  PTTEIKIRRK-CREYMD
                                           PTTEIKIRRK-UPYMD
           MOVE    TEIKI1P-JOBHMS      TO  PTTEIKIRRK-UPHMS
      *
      *    更新前チェック（既に重複するデータがある場合はエラー）
           PERFORM 400-PTTEIKIRRK-CHK-SEC
           IF    ( FLG-PTTEIKIRRK      =   ZERO )
      *
               MOVE    PTTEIKIRRK-PTID TO  ERR-PTID
               MOVE    "0012"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF  (   MCP-RC          NOT =   ZERO )
               MOVE    PTTEIKIRRK-PTID TO  ERR-PTID
               MOVE    "0032"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20031-PTTEIKIRRK-KOUSIN-EXT.
           EXIT. 
      *****************************************************************
      *    日別収納作成処理
      *****************************************************************
       20031-SYUDAY-SAKUSEI-SEC        SECTION.
      *
           INITIALIZE                      WKSYUDAY-REC
      *
           MOVE    TEIKI06-SYUDAY-DAT  TO  WKSYUDAY-REC
           MOVE    SYU-DENPNUM         TO  WKSDAY-DENPNUM
      *
      *    日別収納更新処理
           PERFORM 900-SYUDAY-UPD-SEC
      *
           .
       20031-SYUDAY-SAKUSEI-EXT.
           EXIT.
      *****************************************************************
      *    日別収納削除処理
      *****************************************************************
       20031-SYUDAY-DELETE-SEC         SECTION.
      *
           INITIALIZE                  WKSYUDAY-REC
      *
           MOVE    SYU-HOSPNUM      TO  WKSDAY-HOSPNUM
           MOVE    SYU-NYUGAIKBN   TO  WKSDAY-NYUGAIKBN
           MOVE    SYU-PTID        TO  WKSDAY-PTID
           MOVE    SYU-DENPNUM     TO  WKSDAY-DENPNUM
      *
      *    日別収納更新処理
           PERFORM 900-SYUDAY-DEL-SEC
      *
           .
       20031-SYUDAY-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    収納明細作成処理
      *****************************************************************
       20031-SYUMEI-SAKUSEI-SEC        SECTION.
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *
      *    伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *    伝票番号枝番
           MOVE    01                  TO  SMEI-DENPEDANUM
      *    診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *    伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *    請求発行日（発行日の西暦）
           MOVE    TEIKI1P-SYSYMD      TO  SMEI-SKYPRINTYMD
      *    請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *    領収書発行日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-RYOSYUPRINTYMD
      *    領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *    請求金額
           MOVE    SYU-SKYMONEY        TO  SMEI-SKYMONEY
      *    入返金日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-NYUHEN-YMD
      *    入金連番
           MOVE    01                  TO  SMEI-NYUKINRENNUM
      *
           MOVE    "00"                TO  SMEI-NYUKIN-HOHO
      *
           IF    ( WRK-NYUKIN-KBN  =   "2" )
               IF    ( SYU-NYUKIN-TOTAL    =   ZERO )
                   CONTINUE
               ELSE
      *            入返金区分
                   MOVE    "1"         TO  SMEI-NYUHEN-KBN
      *            入返金額
                   MOVE    SYU-NYUKIN-TOTAL
                                       TO  SMEI-NYUHEN-MONEY
      *            入金方法
                   MOVE    WRK-NYUKIN-HOHO
                                       TO  SMEI-NYUKIN-HOHO
               END-IF
           END-IF
      *
           MOVE    WRK-NYUKIN-HOHO     TO  SMEI-NYUKIN-HOHO
      *
      *    状態区分
           IF    ( SYU-SKYMONEY        =   ZERO )
               MOVE    SPACE               TO  SMEI-JOUTAIKBN
           ELSE
               IF   (  SYU-SKYMONEY    >   SYU-NYUKIN-TOTAL )
                   MOVE    "9"         TO  SMEI-JOUTAIKBN
               ELSE
                   MOVE    "1"         TO  SMEI-JOUTAIKBN
               END-IF
           END-IF

      *
      *    更新前チェック（既に重複するデータがある場合はエラー）
           PERFORM 400-SYUMEI-CHK-SEC
           IF    ( FLG-SYUMEI  =   ZERO )
               MOVE    SMEI-PTID       TO  ERR-PTID
               MOVE    "0022"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    TEIKI1P-JOBYMD      TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    TEIKI1P-JOBHMS      TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           PERFORM 800-SET-SS002-SMEI-SEC
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    SMEI-PTID       TO  ERR-PTID
               MOVE    "0023"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20031-SYUMEI-SAKUSEI-EXT.
           EXIT. 
      *****************************************************************
      *    入金方法編集処理
      *****************************************************************
       200311-NYUKIN-HOHO-HEN-SEC      SECTION.
      *
           MOVE    "01"            TO  WRK-NYUKIN-HOHO
           MOVE    SPACE           TO  WRK-NYUKIN-JYOTAI
           MOVE    SPACE           TO  WRK-NYUKIN-KBN
           MOVE    SPACE           TO  WRK-NYUKIN-BUNRUI
      *
           IF    ( PTINF-NYUKIN-HOHO   NOT =   SPACE )
               MOVE    PTINF-NYUKIN-HOHO
                                   TO  WRK-NYUKIN-HOHO
           END-IF
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-1041-MAX )
                    OR   ( WRK-NYUKIN-JYOTAI   NOT =   SPACE )
      *
               IF    ( WRK-NYUKIN-HOHO =   SRH-1041-KBNCDL (SRH-IDX))
                   MOVE    SRH-1041-JYOTAIL (SRH-IDX)
                                   TO  WRK-NYUKIN-JYOTAI
                   MOVE    SRH-1041-BUNRUIL (SRH-IDX)
                                   TO  WRK-NYUKIN-BUNRUI
               END-IF
      *
           END-PERFORM
      *
           EVALUATE    WRK-NYUKIN-JYOTAI
           WHEN    "1"
               MOVE    "2"             TO  WRK-NYUKIN-KBN
           WHEN    "2"
               MOVE    "1"             TO  WRK-NYUKIN-KBN
           WHEN    "3"
               MOVE    SYS-5010-NYUKINKBN
                                       TO  WRK-NYUKIN-KBN
           WHEN    OTHER
               MOVE    SYS-5010-NYUKINKBN
                                       TO  WRK-NYUKIN-KBN
           END-EVALUATE
           
      *
           .
       200311-NYUKIN-HOHO-HEN-EXT.
           EXIT. 
      *****************************************************************
      *    最新病棟番号別集計処理
      *****************************************************************
       20031-NOW-BTUNUM-SUM-SEC        SECTION.
      *
      *    最新の入院履歴情報より病棟番号を取得
           INITIALIZE  PTNYUINRRK-REC
           MOVE    PTINF-HOSPNUM        TO  PTNYUINRRK-HOSPNUM
           MOVE    PTINF-PTID          TO  PTNYUINRRK-PTID
           MOVE    PTINF-RRKNUM        TO  PTNYUINRRK-RRKNUM
           MOVE    PTINF-RRKEDANUM     TO  PTNYUINRRK-RRKEDANUM
           PERFORM 900-PTNYUINRRK-KEY-SEL-SEC
           IF    ( FLG-PTNYUINRRK  NOT =   ZERO )
               MOVE    PTNYUINRRK-PTID TO  ERR-PTID
               MOVE    "0014"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   50 )
               EVALUATE SUMTEIKIKANRI-BTUNUM (IDX1)
               WHEN    SPACE
                   MOVE    PTNYUINRRK-BTUNUM
                                   TO  SUMTEIKIKANRI-BTUNUM (IDX1)
                   MOVE    1       TO  SUMTEIKIKANRI-KENSU (IDX1)
                   COMPUTE SUMTEIKIKANRI-MAX
                       =   SUMTEIKIKANRI-MAX   +   1
                   MOVE    50      TO  IDX1
               WHEN    PTNYUINRRK-BTUNUM
                   COMPUTE SUMTEIKIKANRI-KENSU (IDX1)
                       =   SUMTEIKIKANRI-KENSU (IDX1)  +   1
                   MOVE    50      TO  IDX1
               END-EVALUATE
           END-PERFORM
           .
      *
       20031-NOW-BTUNUM-SUM-EXT.
           EXIT. 
      *****************************************************************
      *    定期請求印刷情報作成
      *****************************************************************
       20031-TEIKIPRT-SAKUSEI-SEC          SECTION.
      *
           INITIALIZE  TEIKIPRT-REC
           MOVE    PTNYUINRRK-BTUNUM   TO  TEIKIPRT-BTUNUM
           MOVE    PTINF-HOSPNUM       TO  TEIKIPRT-HOSPNUM
           MOVE    PTINF-PTID          TO  TEIKIPRT-PTID
      *
      *    患者番号を取得
           MOVE    PTINF-PTID          TO  WRK-PTID
           PERFORM 900-PTNUM-KEY-SEL-SEC
           IF    ( FLG-PTNUM           NOT =   ZERO )
               MOVE    PTINF-PTID      TO  ERR-PTID
               MOVE    "0015"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           MOVE    PTNUM-PTNUM         TO  TEIKIPRT-PTNUM
      *
           MOVE    PTINF-KANANAME      TO  TEIKIPRT-NAME
           MOVE    SYU-DENPNUM         TO  TEIKIPRT-DENPNUM
      *
      *    更新対象のデータがすでに存在する場合はエラー
           PERFORM 400-TEIKIPRT-CHK-SEC
           IF    ( FLG-TEIKIPRT    =   ZERO )
               MOVE    TEIKIPRT-PTID   TO  ERR-PTID
               MOVE    "0016"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    定期請求印刷データ作成
           MOVE    TEIKIPRT-REC        TO  MCPDATA-REC
           MOVE    "tbl_teikiprt"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF  (   MCP-RC          NOT =   ZERO )
               MOVE    TEIKIPRT-PTID   TO  ERR-PTID
               MOVE    "0037"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20031-TEIKIPRT-SAKUSEI-EXT.
            EXIT.
      *****************************************************************
      *    定期請求情報更新処理（訂正更新）
      *****************************************************************
       2007-SEIKYU-KOUSIN-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TEIKI09
      *
      *    定期請求収納ファイル読込
           PERFORM 910-TEIKI09-READ-SEC
      *
           PERFORM UNTIL ( FLG-TEIKI09 NOT =   ZERO )
      *
               MOVE    TEIKI09-SYU-DAT TO  SYUNOU-REC
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "1001"          TO  SYS-KANRICD
               MOVE    "*"             TO  SYS-KBNCD
               MOVE    SYU-SKYSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1001-REC
               IF    ( SYS-1001-SANKANUM   IS  NOT NUMERIC )
                OR   ( SYS-1001-SANKANUM   =   ZERO    )
                   MOVE    ZERO            TO  SYS-1001-SANKANUM
               END-IF
      *
      *        患者情報読み込み
               INITIALIZE              PTINF-REC
               MOVE    SYU-HOSPNUM      TO  PTINF-HOSPNUM
               MOVE    SYU-PTID        TO  PTINF-PTID
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( FLG-PTINF       NOT =   ZERO )
                   MOVE    PTINF-PTID  TO  ERR-PTID
                   MOVE    "0036"      TO  ERR-ERRCD
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
               PERFORM 200311-NYUKIN-HOHO-HEN-SEC
      *
      *        更新前チェック（更新対象の収納がなければエラー）
               PERFORM 400-SYUNOU-CHK-SEC
               IF    ( FLG-SYUNOU  NOT =   ZERO )
                   MOVE    SYU-PTID    TO  ERR-PTID
                   MOVE    "0029"      TO  ERR-ERRCD
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
               PERFORM 20071-SYUNOU-HEN-SEC
      *
               MOVE    TEIKI1P-JOBYMD      TO  SYU-UPYMD
               MOVE    TEIKI1P-JOBHMS      TO  SYU-UPHMS
      *
               PERFORM 800-SS002-SEC
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    SYU-PTID        TO  ERR-PTID
                   MOVE    "0030"      TO  ERR-ERRCD
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
      *        日別収納作成処理
               IF    ( TEIKI09-SYUDAY-EXIST-FLG    =   1 )
                   PERFORM 20071-SYUDAY-SAKUSEI-SEC
               ELSE
                   PERFORM 20071-SYUDAY-DELETE-SEC
               END-IF
      *
      *        収納明細作成処理
               PERFORM 20071-SYUMEI-SAKUSEI-SEC
      *
      *        患者定期請求履歴更新処理
               PERFORM 20071-PTTEIKIRRK-KOUSIN-SEC
      *
      *        最新病棟番号別集計処理
               PERFORM 20031-NOW-BTUNUM-SUM-SEC
      *
      *        分娩情報更新処理
               PERFORM 20081-BUNBEN-KOUSIN-SEC
      *
      *        定期請求印刷情報作成処理
               PERFORM 20031-TEIKIPRT-SAKUSEI-SEC
      *
      *        定期請求収納ファイル読込
               PERFORM 910-TEIKI09-READ-SEC
      *
           END-PERFORM
      *
           .
       2007-SEIKYU-KOUSIN-EXT.
           EXIT. 
      *****************************************************************
      *    収納マスタ編集処理
      *****************************************************************
       20071-SYUNOU-HEN-SEC            SECTION.
      *
           MOVE    TEIKI09-SYU-DAT     TO  SYUNOU-REC
      *
      *    入金済みとして処理する場合、請求額の差分がプラスで計上される
      *    場合は同額を入金額に加算する
           IF    ( WRK-NYUKIN-KBN  =   "2" )
               IF    ( TEIKI09-TSI-SKYMONEY    >   ZERO )
      *
                   COMPUTE SYU-NYUKIN-TOTAL
                       =   SYU-NYUKIN-TOTAL
                       +   TEIKI09-TSI-SKYMONEY
      *
                   COMPUTE SYU-NYUKIN-KAISU
                       =   SYU-NYUKIN-KAISU
                       +   1
               END-IF
           END-IF
      *
           IF    ( SYU-SKYMONEY        >   SYU-NYUKIN-TOTAL )
               MOVE    "2"             TO  SYU-DENPJTIKBN
           ELSE
               MOVE    "1"             TO  SYU-DENPJTIKBN
           END-IF
      *
           .
       20071-SYUNOU-HEN-EXT.
           EXIT. 
      *****************************************************************
      *    日別収納作成処理
      *****************************************************************
       20071-SYUDAY-SAKUSEI-SEC        SECTION.
      *
           INITIALIZE                      WKSYUDAY-REC
      *
           MOVE    TEIKI09-SYUDAY-DAT  TO  WKSYUDAY-REC
           MOVE    SYU-DENPNUM         TO  WKSDAY-DENPNUM
      *
      *    日別収納更新処理
           PERFORM 900-SYUDAY-UPD-SEC
      *
           .
       20071-SYUDAY-SAKUSEI-EXT.
           EXIT.
      *****************************************************************
      *    日別収納削除処理
      *****************************************************************
       20071-SYUDAY-DELETE-SEC         SECTION.
      *
           INITIALIZE                  WKSYUDAY-REC
      *
           MOVE    SYU-HOSPNUM      TO  WKSDAY-HOSPNUM
           MOVE    SYU-NYUGAIKBN   TO  WKSDAY-NYUGAIKBN
           MOVE    SYU-PTID        TO  WKSDAY-PTID
           MOVE    SYU-DENPNUM     TO  WKSDAY-DENPNUM
      *
      *    日別収納更新処理
           PERFORM 900-SYUDAY-DEL-SEC
      *
           .
       20071-SYUDAY-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴データ更新処理
      *****************************************************************
       20071-PTTEIKIRRK-KOUSIN-SEC     SECTION.
      *
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    SYU-HOSPNUM          TO  PTTEIKIRRK-HOSPNUM
           MOVE    SYU-PTID            TO  PTTEIKIRRK-PTID
           MOVE    SYU-DENPNUM         TO  PTTEIKIRRK-DENPNUM
      *
      *    更新対象のデータを取得（更新対象のデータがなければエラー）
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
           PERFORM 900-PTTEIKIRRK-KEY6-SEL-SEC
           IF    ( FLG-PTTEIKIRRK  NOT =   ZERO )
               MOVE    PTTEIKIRRK-PTID TO  ERR-PTID
               MOVE    "0033"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    患者定期請求履歴削除処理
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( FLG-PTTEIKIRRK  NOT =   ZERO )
               MOVE    PTTEIKIRRK-PTID TO  ERR-PTID
               MOVE    "0034"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    PTTEIKIRRK-CREYMD    TO  WRK-CREYMD
      *
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    SYU-HOSPNUM          TO  PTTEIKIRRK-HOSPNUM
           MOVE    SYU-PTID            TO  PTTEIKIRRK-PTID
           MOVE    SYU-SRYYMD (1:6)    TO  PTTEIKIRRK-SRYYM
           MOVE    TEIKI09-SAKKBN      TO  PTTEIKIRRK-SAKKBN
           MOVE    SYU-HKNCOMBINUM     TO  PTTEIKIRRK-HKNCOMBINUM
           MOVE    ZERO                TO  PTTEIKIRRK-DELFLG
           MOVE    TEIKI1P-SYOKBN      TO  PTTEIKIRRK-SYOKBN
           MOVE    TEIKI09-KBT         TO  PTTEIKIRRK-KBT
           MOVE    TEIKI09-SKYSTYMD    TO  PTTEIKIRRK-SKYSTYMD
           MOVE    TEIKI09-SKYEDYMD    TO  PTTEIKIRRK-SKYEDYMD
      *
           MOVE    TEIKI09-TSI-SKYTEN
                                       TO  PTTEIKIRRK-SKYTEN
      *
           MOVE    TEIKI09-TSI-SKYMONEY-TAX-SAI
                                       TO  PTTEIKIRRK-SKYMONEY-TAX-SAI
      *
           MOVE    TEIKI09-TSI-SKYMONEY
                                       TO  PTTEIKIRRK-SKYMONEY
      *
           MOVE    SYU-NYUIN-RRKNUM    TO  PTTEIKIRRK-RRKNUM
           MOVE    SYU-DENPNUM         TO  PTTEIKIRRK-DENPNUM
           MOVE    SMEI-DENPEDANUM     TO  PTTEIKIRRK-DENPEDANUM
      *
           MOVE    1                   TO  PTTEIKIRRK-SKYUPDFLG
      *
           MOVE    WRK-CREYMD          TO  PTTEIKIRRK-CREYMD
      *
           MOVE    TEIKI1P-JOBYMD      TO  PTTEIKIRRK-UPYMD
           MOVE    TEIKI1P-JOBHMS      TO  PTTEIKIRRK-UPHMS
      *
      *    患者定期請求データ作成
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF  (   MCP-RC          NOT =   ZERO )
               MOVE    PTTEIKIRRK-PTID TO  ERR-PTID
               MOVE    "0035"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20071-PTTEIKIRRK-KOUSIN-EXT.
           EXIT. 
      *****************************************************************
      *    収納明細作成処理
      *****************************************************************
       20071-SYUMEI-SAKUSEI-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-DENPEDANUM
           MOVE    ZERO                TO  WRK-NYUKINRENNUM
      *
           PERFORM 900-SYUMEI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUMEI  NOT =   ZERO )
      *
               IF    ( SMEI-DENPEDANUM     >   WRK-DENPEDANUM )
                   MOVE    SMEI-DENPEDANUM     TO  WRK-DENPEDANUM
               END-IF
      *
               IF    ( SMEI-NYUKINRENNUM   >   WRK-NYUKINRENNUM )
                   MOVE    SMEI-NYUKINRENNUM   TO  WRK-NYUKINRENNUM
               END-IF
      *
      *        診療科変更
               IF    ( SYU-SRYKA       NOT =   SMEI-SRYKA )
      *
                   MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *
                   MOVE    TEIKI1P-JOBYMD      TO  SMEI-UPYMD
                   MOVE    TEIKI1P-JOBHMS      TO  SMEI-UPHMS
      *
                   MOVE    SYUMEI-REC          TO  MCPDATA-REC
                   MOVE    "tbl_syumei"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC          NOT =   ZERO )
                       MOVE    SMEI-PTID       TO  ERR-PTID
                       MOVE    "0024"          TO  ERR-ERRCD
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-SYUMEI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           INITIALIZE                      SYUMEI-REC
      *
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *
           COMPUTE WRK-DENPEDANUM
               =   WRK-DENPEDANUM      +   1
      *
           MOVE    WRK-DENPEDANUM      TO  SMEI-DENPEDANUM
      *
           COMPUTE WRK-NYUKINRENNUM
               =   WRK-NYUKINRENNUM    +   1
      *
           MOVE    WRK-NYUKINRENNUM    TO  SMEI-NYUKINRENNUM
      *
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *
           MOVE    TEIKI1P-SYSYMD      TO  SMEI-SKYPRINTYMD
      *
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *
           MOVE    TEIKI09-TSI-SKYMONEY
                                       TO  SMEI-SKYMONEY
      *
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-NYUHEN-YMD 
      *
           MOVE    "00"                TO  SMEI-NYUKIN-HOHO
      *
      *    入金額がゼロの時、入金力なし
           IF    ( WRK-NYUKIN-KBN  =   "2" )
               IF    ( TEIKI09-TSI-SKYMONEY >   ZERO )
                   MOVE    "1"         TO  SMEI-NYUHEN-KBN
                   MOVE    TEIKI09-TSI-SKYMONEY
                                       TO  SMEI-NYUHEN-MONEY
                   MOVE    WRK-NYUKIN-HOHO
                                       TO  SMEI-NYUKIN-HOHO
               END-IF
           END-IF
      *
           MOVE    "8"                 TO  SMEI-JOUTAIKBN
      *
      *    更新前チェック（既に重複するデータがある場合はエラー）
           PERFORM 400-SYUMEI-CHK-SEC
           IF    ( FLG-SYUMEI  =   ZERO )
               MOVE    SMEI-PTID       TO  ERR-PTID
               MOVE    "0025"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    TEIKI1P-JOBYMD      TO  SMEI-CREYMD
                                           SMEI-UPYMD
           MOVE    TEIKI1P-JOBHMS      TO  SMEI-CREHMS
                                           SMEI-UPHMS
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           PERFORM 800-SET-SS002-SMEI-SEC
      *
           MOVE    SYUMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    SMEI-PTID       TO  ERR-PTID
               MOVE    "0026"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20071-SYUMEI-SAKUSEI-EXT.
           EXIT. 
      *****************************************************************
      *    分娩管理更新処理
      *****************************************************************
       20081-BUNBEN-KOUSIN-SEC         SECTION.
      *
           PERFORM 900-BUNBEN-KEY3-SEL-SEC
           IF    ( FLG-BUNBEN          =   ZERO )
               IF    ( BUNBEN-UPDKBN       =   "1" OR "2" )
                   PERFORM 200811-BUNBEN-KOUSIN-SEC
               END-IF
           END-IF
      *
           .
       20081-BUNBEN-KOUSIN-EXT.
           EXIT. 
      *****************************************************************
      *    分娩管理更新処理
      *****************************************************************
       200811-BUNBEN-KOUSIN-SEC        SECTION.
      *
           PERFORM 800-ORCSBUNBENFTN-SEC
      *
           MOVE    BUNBEN-RIYOUKBN     TO  LNKBUNBEN-RIYOUKBN
           MOVE    BUNBEN-SKYYM        TO  LNKBUNBEN-SKYYM
           MOVE    BUNBEN-SKYKBN       TO  LNKBUNBEN-SKYKBN
           MOVE    BUNBEN-BUNBENKBN    TO  LNKBUNBEN-BUNBENKBN
           MOVE    BUNBEN-TEISYUTUSAKI TO  LNKBUNBEN-TEISYUTUSAKI
           MOVE    BUNBEN-KANYUKBN     TO  LNKBUNBEN-KANYUKBN
           MOVE    BUNBEN-HONKZKKBN    TO  LNKBUNBEN-HONKZKKBN
           MOVE    BUNBEN-HKNJANUM     TO  LNKBUNBEN-HKNJANUM
           MOVE    BUNBEN-HKNJANUM-S   TO  LNKBUNBEN-HKNJANUM-S
           MOVE    BUNBEN-KIGO         TO  LNKBUNBEN-KIGO
           MOVE    BUNBEN-NUM          TO  LNKBUNBEN-NUM
           MOVE    BUNBEN-BIRTHDAY     TO  LNKBUNBEN-BIRTHDAY
           MOVE    BUNBEN-ZAIWEEK      TO  LNKBUNBEN-ZAIWEEK
           MOVE    BUNBEN-SIZANKBN     TO  LNKBUNBEN-SIZANKBN
           MOVE    BUNBEN-BIRTHCNT     TO  LNKBUNBEN-BIRTHCNT
           MOVE    BUNBEN-NYUIN-DAY    TO  LNKBUNBEN-NYUIN-DAY
           MOVE    BUNBEN-SANKAKBN     TO  LNKBUNBEN-SANKAKBN
           MOVE    BUNBEN-BIKOU        TO  LNKBUNBEN-BIKOU
           MOVE    "2"                 TO  LNKBUNBEN-UPDKBN
           MOVE    BUNBEN-CREYMD       TO  LNKBUNBEN-CREYMD
           MOVE    BUNBEN-TEISYUTUBIKBN TO LNKBUNBEN-TEISYUTUBIKBN
      *
           MOVE    LNKBUNBEN-REC       TO  BUNBEN-REC
      *
           MOVE    ZERO                TO  BUNBEN-DAIRI-MONEY
           MOVE    ZERO                TO  WRK-BASE-MONEY
           IF    ( BUNBEN-RIYOUKBN     =   "1" )
      *
               EVALUATE    TRUE
               WHEN  ( SYS-1001-SANKANUM    =   ZERO )
               WHEN  ( BUNBEN-ZAIWEEK     <   22 )
               WHEN  ( BUNBEN-SANKAKBN    =   "2" OR "3" )
                   IF    ( BUNBEN-BIRTHDAY   NOT =   SPACE )
                       EVALUATE   TRUE
                       WHEN    ( BUNBEN-BIRTHDAY
                                          >= CONST-H270101 )
                           MOVE    404000  TO  WRK-BASE-MONEY
                       WHEN    OTHER
                           MOVE    390000  TO  WRK-BASE-MONEY
                       END-EVALUATE
                   END-IF
               WHEN    OTHER
                   MOVE    420000      TO  WRK-BASE-MONEY
               END-EVALUATE
      *
               IF    ( BUNBEN-SANKAKBN    =   "3" )
                   COMPUTE BUNBEN-DAIRI-MONEY
                       =   WRK-BASE-MONEY  *   BUNBEN-BIRTHCNT
                       +   BUNBEN-SNK-MONEY
               ELSE
                   COMPUTE BUNBEN-DAIRI-MONEY
                       =   WRK-BASE-MONEY  *   BUNBEN-BIRTHCNT
               END-IF
      *
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    TEIKI1P-JOBYMD      TO  BUNBEN-UPYMD
           MOVE    TEIKI1P-JOBHMS      TO  BUNBEN-UPHMS
      *
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          =   ZERO )
               CONTINUE
           ELSE
               MOVE    BUNBEN-PTID TO  ERR-PTID
               MOVE    "0044"      TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    BUNBEN-REC      TO  MCPDATA-REC
               MOVE    "tbl_bunben"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    BUNBEN-PTID TO  ERR-PTID
                   MOVE    "0045"      TO  ERR-ERRCD
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       200811-BUNBEN-KOUSIN-EXT.
           EXIT. 
      *****************************************************************
      *    定期請求管理情報更新処理
      *****************************************************************
       2005-TEIKIKANRI-KOUSIN-SEC      SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SUMTEIKIKANRI-MAX )
      *
               INITIALIZE  TEIKIKANRI-REC
      *
               MOVE    TEIKI1P-HOSPNUM
                                   TO  TEIKIKANRI-HOSPNUM
               MOVE    SUMTEIKIKANRI-BTUNUM (IDX1)
                                   TO  TEIKIKANRI-BTUNUM
      *
               PERFORM 400-TEIKIKANRI-CHK-SEC
               IF    ( FLG-TEIKIKANRI  =   ZERO )
      *
                   COMPUTE TEIKIKANRI-PAGE
                       =   TEIKIKANRI-PAGE
                       +   SUMTEIKIKANRI-KENSU (IDX1)
      *
                   COMPUTE TEIKIKANRI-KENSU
                       =   TEIKIKANRI-KENSU
                       +   SUMTEIKIKANRI-KENSU (IDX1)
      *
                   MOVE    TEIKI1P-HOSPNUM     TO  TEIKIKANRI-HOSPNUM
      *
      *            定期請求管理データ作成
                   MOVE    TEIKIKANRI-REC      TO  MCPDATA-REC
                   MOVE    "tbl_teikikanri"    TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF  (   MCP-RC          NOT =   ZERO )
                       MOVE    "0039"          TO  ERR-ERRCD
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
      *
               ELSE
                   INITIALIZE  TEIKIKANRI-REC
      *
                   MOVE    SUMTEIKIKANRI-BTUNUM (IDX1)
                                       TO  TEIKIKANRI-BTUNUM
                   MOVE    TEIKI1P-SYOKBN
                                       TO  TEIKIKANRI-SYOKBN
      *
                   MOVE    TEIKI1P-JOBYMD
                                       TO  TEIKIKANRI-CREYMD
                   MOVE    TEIKI1P-JOBHMS
                                       TO  TEIKIKANRI-CREHMS
      *
                   IF    ( TEIKI1P-SYOKBN =   "1" )
                       MOVE    TEIKI1P-SRYYM
                                       TO  TEIKIKANRI-SRYYM
                   END-IF
      *
                   MOVE    SUMTEIKIKANRI-KENSU (IDX1)
                                           TO  TEIKIKANRI-PAGE
                                               TEIKIKANRI-KENSU
                   MOVE    TEIKI1P-TERMID  TO  TEIKIKANRI-TERMID
      *
                   MOVE    TEIKI1P-HOSPNUM TO  TEIKIKANRI-HOSPNUM
      *
      *            定期請求管理データ作成
                   MOVE    TEIKIKANRI-REC      TO  MCPDATA-REC
                   MOVE    "tbl_teikikanri"    TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF  (   MCP-RC          NOT =   ZERO )
                       MOVE    "0038"          TO  ERR-ERRCD
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       2005-TEIKIKANRI-KOUSIN-EXT.
           EXIT. 
      *****************************************************************
      *    定期請求履歴情報更新処理
      *****************************************************************
       2006-TEIKIRRK-KOUSIN-SEC        SECTION.
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   3 )
      *
               IF    ( TEIKI1P-KIKAN-KBN (IDX4)    =   "1" )
                AND  ( TEIKI1P-KBT =      IDX4 )
      *
                   INITIALIZE                  TEIKIRRK-REC
                   MOVE    TEIKI1P-HOSPNUM TO  TEIKIRRK-HOSPNUM
                   MOVE    TEIKI1P-SRYYM   TO  TEIKIRRK-SRYYM
                   MOVE    IDX4            TO  WRK-KBT-9
                   MOVE    WRK-KBT         TO  TEIKIRRK-KBT
                   PERFORM 900-TEIKIRRK-KEY-SEL-SEC
                   IF    ( FLG-TEIKIRRK    =   ZERO )
                       MOVE    TEIKI1P-JOBYMD  TO  TEIKIRRK-UPYMD
                       MOVE    TEIKI1P-JOBHMS  TO  TEIKIRRK-UPHMS
                       MOVE    TEIKIRRK-REC    TO  MCPDATA-REC
                       MOVE    "tbl_teikirrk"  TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBUPDATE-SEC
                       IF  (   MCP-RC          NOT =   ZERO )
                           MOVE    "0018"      TO  ERR-ERRCD
                           PERFORM 500-ERR-HENSYU-SEC
                       END-IF
                   ELSE
                       INITIALIZE                  TEIKIRRK-REC
                       MOVE    TEIKI1P-HOSPNUM TO  TEIKIRRK-HOSPNUM
                       MOVE    TEIKI1P-SRYYM   TO  TEIKIRRK-SRYYM
                       MOVE    IDX4            TO  WRK-KBT-9
                       MOVE    WRK-KBT         TO  TEIKIRRK-KBT
                       MOVE    TEIKI1P-JOBYMD  TO  TEIKIRRK-CREYMD
                                                   TEIKIRRK-UPYMD
                       MOVE    TEIKI1P-JOBHMS  TO  TEIKIRRK-UPHMS
                       MOVE    TEIKIRRK-REC    TO  MCPDATA-REC
                       MOVE    "tbl_teikirrk"  TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBINSERT-SEC
                       IF  (   MCP-RC          NOT =   ZERO )
                           MOVE    "0019"      TO  ERR-ERRCD
                           PERFORM 500-ERR-HENSYU-SEC
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       2006-TEIKIRRK-KOUSIN-EXT.
           EXIT. 
      *****************************************************************
      *    単独更新用日別収納更新処理
      *****************************************************************
       2004-TEIKI08-KOUSIN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TEIKI08
           PERFORM 910-TEIKI08-READ-SEC
      *
           PERFORM UNTIL ( FLG-TEIKI08 NOT =   ZERO )
      *
               MOVE    TEIKI08-SYUDAY-DAT
                                       TO  WKSYUDAY-REC
      *
               EVALUATE    TEIKI08-UPD-FLG
      *
      *        更新
               WHEN    1
                   PERFORM 900-SYUDAY-UPD-SEC
      *
                   INITIALIZE  SYUNOU-REC
                   MOVE    WKSDAY-HOSPNUM       TO  SYU-HOSPNUM
                   MOVE    WKSDAY-NYUGAIKBN    TO  SYU-NYUGAIKBN
                   MOVE    WKSDAY-PTID         TO  SYU-PTID
                   MOVE    WKSDAY-DENPNUM      TO  SYU-DENPNUM
                   PERFORM 900-SYUNOU-KEY-SEL-SEC
                   IF    ( FLG-SYUNOU  NOT =   ZERO )
                       MOVE    WKSDAY-PTID TO  ERR-PTID
                       MOVE    "0040"      TO  ERR-ERRCD
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
      *
                   MOVE    "1"             TO  SYU-DAYINFFLG
                   MOVE    SYUNOU-REC      TO  MCPDATA-REC
                   MOVE    "tbl_syunou"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC          NOT =   ZERO )
                       MOVE    SYU-PTID    TO  ERR-PTID
                       MOVE    "0041"      TO  ERR-ERRCD
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
      *
      *        削除
               WHEN    2
                   PERFORM 900-SYUDAY-DEL-SEC
      *
                   INITIALIZE  SYUNOU-REC
                   MOVE    WKSDAY-HOSPNUM       TO  SYU-HOSPNUM
                   MOVE    WKSDAY-NYUGAIKBN    TO  SYU-NYUGAIKBN
                   MOVE    WKSDAY-PTID         TO  SYU-PTID
                   MOVE    WKSDAY-DENPNUM      TO  SYU-DENPNUM
                   PERFORM 900-SYUNOU-KEY-SEL-SEC
                   IF    ( FLG-SYUNOU  NOT =   ZERO )
                       MOVE    WKSDAY-PTID TO  ERR-PTID
                       MOVE    "0042"      TO  ERR-ERRCD
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
      *
                   MOVE    SPACE           TO  SYU-DAYINFFLG
                   MOVE    SYUNOU-REC      TO  MCPDATA-REC
                   MOVE    "tbl_syunou"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC          NOT =   ZERO )
                       MOVE    SYU-PTID    TO  ERR-PTID
                       MOVE    "0043"      TO  ERR-ERRCD
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
      *
               END-EVALUATE
      *
               PERFORM 910-TEIKI08-READ-SEC
      *
           END-PERFORM
      *
           .
       2004-TEIKI08-KOUSIN-EXT.
           EXIT. 
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           PERFORM 800-TEIKIERR-WRITE-SEC
      *
           MOVE    1                   TO  FLG-ERR
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
      *    プログラム終了処理
           PERFORM 990-PROGRAM-FINISH-SEC
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   TEIKI06-FILE
                   TEIKI07-FILE
                   TEIKI08-FILE
                   TEIKI09-FILE
      *
           IF    ( CNT-TEIKI06      =   ZERO )
            AND  ( CNT-TEIKI09      =   ZERO )
            AND  ( CNT-TEIKI08      >   ZERO )
               MOVE    1            TO   FLG-WARNING
           END-IF
      *
      *    ステップ管理終了処理
           INITIALIZE                  JOBKANRI-REC
           MOVE    TEIKI1P-JOBID   TO  JOB-JOBID
           MOVE    TEIKI1P-SHELLID
                                   TO  JOB-SHELLID
           COMPUTE JOB-UPDCNT
               =   CNT-TEIKI06
               +   CNT-TEIKI09
      *
           IF    ( FLG-WARNING     =   ZERO )
               MOVE    "処理は正常に終了しました"
                                   TO  JOB-YOBI
           END-IF
      *
           MOVE    "STM"           TO  SJOBKANRI-MODE
           MOVE    TEIKI1P-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    "STE"           TO  SJOBKANRI-MODE
           MOVE    TEIKI1P-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           IF    ( FLG-WARNING     =   1 )
               MOVE    1           TO  ERR-WARNING
               MOVE    "0020"      TO  ERR-ERRCD
               PERFORM 800-TEIKIERR-WRITE-SEC
           END-IF
      *
           DISPLAY "TEIKI06 (ORCT0040) CNT " CNT-TEIKI06
           DISPLAY "TEIKI09 (ORCT0040) CNT " CNT-TEIKI09
           DISPLAY "TEIKI08 (ORCT0040) CNT " CNT-TEIKI08
           DISPLAY "*** ORCT0040 END ***"
      *
      ***  ACCEPT  WRK-IN FROM CONSOLE 
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦変換処理
      *****************************************************************
       800-WAREKI-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD-G.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
      *
      *
           .
       800-WAREKI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター存在チェック
      *****************************************************************
       400-SYUNOU-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       400-SYUNOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納明細存在チェック
      *****************************************************************
       400-SYUMEI-CHK-SEC              SECTION.
      *
      *    追加対象のデータが既に存在しないかチェック
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       400-SYUMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴存在チェック
      *****************************************************************
       400-PTTEIKIRRK-CHK-SEC          SECTION.
      *
           MOVE   ZERO                 TO  FLG-PTTEIKIRRK
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-PTTEIKIRRK
           END-IF
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       400-PTTEIKIRRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日別収納マスタ検索処理
      *****************************************************************
       400-SYUDAY-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUDAY
      *
           MOVE    SYUDAY-REC          TO  MCPDATA-REC
           MOVE    "tbl_syuday"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SYUDAY
           END-IF
      *
           MOVE    "tbl_syuday"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       400-SYUDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求印刷情報存在チェック処理
      *****************************************************************
       400-TEIKIPRT-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-TEIKIPRT
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKIPRT-HOSPNUM
           MOVE    TEIKIPRT-REC        TO  MCPDATA-REC
           MOVE    "tbl_teikiprt"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-TEIKIPRT
           END-IF
      *
           MOVE    "tbl_teikiprt"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       400-TEIKIPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求管理情報存在チェック処理
      *****************************************************************
       400-TEIKIKANRI-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TEIKIKANRI
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKIKANRI-HOSPNUM
           MOVE    TEIKIKANRI-REC      TO  MCPDATA-REC
           MOVE    "tbl_teikikanri"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEIKIKANRI-REC
           ELSE
               MOVE    1               TO  FLG-TEIKIKANRI
           END-IF
      *
           MOVE    "tbl_teikikanri"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       400-TEIKIKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラーファイル出力処理
      *****************************************************************
       800-TEIKIERR-WRITE-SEC          SECTION.
      *
           PERFORM 800-GET-ERRMSG-SEC
      *
           IF    ( ERR-PTID        NOT =   ZERO )
               PERFORM 800-EDIT-ERRMSG-SEC
           END-IF
      *
           OPEN    OUTPUT              TEIKIERR-FILE
      *
           INITIALIZE                  TEIKIERR-REC
           MOVE    ERR-ERRCD           TO  TEIKIERR-ERRCD
           MOVE    ERR-WARNING         TO  TEIKIERR-WARNING
           MOVE    ERR-ERRMSG          TO  TEIKIERR-ERRMSG
           WRITE   TEIKIERR-REC
           CLOSE   TEIKIERR-FILE
      *
           .
       800-TEIKIERR-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    分娩負担金サブ
      *****************************************************************
       800-ORCSBUNBENFTN-SEC           SECTION.
      *
           INITIALIZE                  LNKBUNBEN-REC
           MOVE    SYU-HOSPNUM     TO  LNKBUNBEN-HOSPNUM
           MOVE    SYU-PTID        TO  LNKBUNBEN-PTID
           MOVE    SYU-NYUGAIKBN   TO  LNKBUNBEN-NYUGAIKBN
           MOVE    SYU-NYUIN-RRKNUM
                                   TO  LNKBUNBEN-RRKNUM
           CALL    "ORCSBUNBENFTN"     USING
                                       LNKBUNBEN-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSBUNBENFTN-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ取得処理
      *****************************************************************
       800-GET-ERRMSG-SEC              SECTION.
      *
           EVALUATE    ERR-ERRCD
           WHEN    "0001"
               MOVE    ERR-MSG01       TO  ERR-ERRMSG
           WHEN    "0002"
               MOVE    ERR-MSG02       TO  ERR-ERRMSG
           WHEN    "0003"
               MOVE    ERR-MSG03       TO  ERR-ERRMSG
           WHEN    "0004"
               MOVE    ERR-MSG04       TO  ERR-ERRMSG
           WHEN    "0005"
               MOVE    ERR-MSG05       TO  ERR-ERRMSG
           WHEN    "0006"
               MOVE    ERR-MSG06       TO  ERR-ERRMSG
           WHEN    "0007"
               MOVE    ERR-MSG07       TO  ERR-ERRMSG
           WHEN    "0008"
               MOVE    ERR-MSG08       TO  ERR-ERRMSG
           WHEN    "0009"
      *
               MOVE    SPACE           TO  ERR-ERRMSG
               STRING  ERR-MSG09      DELIMITED    BY  SIZE
                       "[伝票番号:"   DELIMITED    BY  SIZE
                       SYS-0042-DENPNUMMAX-NYU
                                      DELIMITED    BY  SIZE
                       "]"            DELIMITED    BY  SIZE
               INTO    ERR-ERRMSG
               END-STRING
           WHEN    "0010"
               MOVE    ERR-MSG10       TO  ERR-ERRMSG
           WHEN    "0011"
               MOVE    ERR-MSG11       TO  ERR-ERRMSG
           WHEN    "0012"
               MOVE    ERR-MSG12       TO  ERR-ERRMSG
           WHEN    "0013"
               MOVE    ERR-MSG13       TO  ERR-ERRMSG
           WHEN    "0014"
               MOVE    ERR-MSG14       TO  ERR-ERRMSG
           WHEN    "0015"
               MOVE    ERR-MSG15       TO  ERR-ERRMSG
           WHEN    "0016"
               MOVE    ERR-MSG16       TO  ERR-ERRMSG
           WHEN    "0017"
               MOVE    ERR-MSG17       TO  ERR-ERRMSG
           WHEN    "0018"
               MOVE    ERR-MSG18       TO  ERR-ERRMSG
           WHEN    "0019"
               MOVE    ERR-MSG19       TO  ERR-ERRMSG
           WHEN    "0020"
               MOVE    ERR-MSG20       TO  ERR-ERRMSG
           WHEN    "0021"
               MOVE    ERR-MSG21       TO  ERR-ERRMSG
           WHEN    "0022"
               MOVE    ERR-MSG22       TO  ERR-ERRMSG
           WHEN    "0023"
               MOVE    ERR-MSG23       TO  ERR-ERRMSG
           WHEN    "0024"
               MOVE    ERR-MSG24       TO  ERR-ERRMSG
           WHEN    "0025"
               MOVE    ERR-MSG25       TO  ERR-ERRMSG
           WHEN    "0026"
               MOVE    ERR-MSG26       TO  ERR-ERRMSG
           WHEN    "0027"
               MOVE    ERR-MSG27       TO  ERR-ERRMSG
           WHEN    "0028"
               MOVE    ERR-MSG28       TO  ERR-ERRMSG
           WHEN    "0029"
               MOVE    ERR-MSG29       TO  ERR-ERRMSG
           WHEN    "0030"
               MOVE    ERR-MSG30       TO  ERR-ERRMSG
           WHEN    "0031"
               MOVE    ERR-MSG31       TO  ERR-ERRMSG
           WHEN    "0032"
               MOVE    ERR-MSG32       TO  ERR-ERRMSG
           WHEN    "0033"
               MOVE    ERR-MSG33       TO  ERR-ERRMSG
           WHEN    "0034"
               MOVE    ERR-MSG34       TO  ERR-ERRMSG
           WHEN    "0035"
               MOVE    ERR-MSG35       TO  ERR-ERRMSG
           WHEN    "0036"
               MOVE    ERR-MSG36       TO  ERR-ERRMSG
           WHEN    "0037"
               MOVE    ERR-MSG37       TO  ERR-ERRMSG
           WHEN    "0038"
               MOVE    ERR-MSG38       TO  ERR-ERRMSG
           WHEN    "0039"
               MOVE    ERR-MSG39       TO  ERR-ERRMSG
           WHEN    "0040"
               MOVE    ERR-MSG40       TO  ERR-ERRMSG
           WHEN    "0041"
               MOVE    ERR-MSG41       TO  ERR-ERRMSG
           WHEN    "0042"
               MOVE    ERR-MSG42       TO  ERR-ERRMSG
           WHEN    "0043"
               MOVE    ERR-MSG43       TO  ERR-ERRMSG
           WHEN    "0044"
               MOVE    ERR-MSG44       TO  ERR-ERRMSG
           WHEN    "0045"
               MOVE    ERR-MSG45       TO  ERR-ERRMSG
           WHEN    "8888"
               MOVE    ERR-MSG88       TO  ERR-ERRMSG
           END-EVALUATE
      *
           .
       800-GET-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       800-EDIT-ERRMSG-SEC             SECTION.
      *
           MOVE    SPACE               TO  ERR-HEN-ERRMSG
	                                   ERR-PTNUM
      *
      *    患者番号を取得
           MOVE    ERR-PTID            TO  WRK-PTID
           PERFORM 900-PTNUM-KEY-SEL-SEC
           IF    ( FLG-PTNUM           =   ZERO )
               MOVE    PTNUM-PTNUM     TO  ERR-PTNUM
           ELSE
               STRING "?(ID:"          DELIMITED   BY  SIZE
                      ERR-PTID         DELIMITED   BY  SIZE
                      ")"              DELIMITED   BY  SIZE
               INTO   ERR-PTNUM
               END-STRING
           END-IF
      *
           MOVE    1                   TO  WRK-LEN
           MOVE    SPACE               TO  WRK-STR
           STRING  ERR-PTNUM           DELIMITED   BY  SPACE
           INTO    WRK-STR
           WITH  POINTER WRK-LEN
           END-STRING
      *
           COMPUTE WRK-LEN =   WRK-LEN -   1
      *
           MOVE    ZERO                TO  WRK-SHO
                                           WRK-AMARI
           MOVE    SPACE               TO  WRK-KUGIRI
      *
           DIVIDE  2   INTO    WRK-LEN GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI   =   ZERO )
               MOVE    "："            TO  WRK-KUGIRI
           ELSE
               MOVE    ":"             TO  WRK-KUGIRI
           END-IF
      *
           STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                   "［患者番号"        DELIMITED   BY  SIZE
                   WRK-KUGIRI          DELIMITED   BY  SPACE
                   ERR-PTNUM           DELIMITED   BY  SPACE
                   "］"                DELIMITED   BY  SIZE
           INTO    ERR-HEN-ERRMSG
           END-STRING
      *
           MOVE    ERR-HEN-ERRMSG      TO  ERR-ERRMSG
      *
           .
       800-EDIT-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    日報キー取得サブ
      *****************************************************************
       800-DAILYKEY-SEC         SECTION.
      *
           CALL    "ORCSDAILYKEY"   USING   SDAILYKEYAREA
                                            SPA-AREA
      *
           .
      *
       800-DAILYKEY-EXT.
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
      *    収納更新差額取得サブ
      *****************************************************************
       800-SS002-SEC           SECTION.
      *
           INITIALIZE                      SS002-AREA
           CALL    "ORCSS002"      USING   SS002-AREA
                                           SYUNOU-REC
                                           SPA-AREA
      *
           .
      *
       800-SS002-EXT.
           EXIT.
      *****************************************************************
      *    収納明細更新差額設定処理
      *****************************************************************
       800-SET-SS002-SMEI-SEC      SECTION.
      *
           MOVE    SS002-SYURRKNUM     TO  SMEI-SYURRKNUM
           MOVE    SS002-SYUEDANUM     TO  SMEI-SYUEDANUM
           MOVE    SS002-SYURRKUPDKBN  TO  SMEI-SYURRKUPDKBN
      *
           .
      *
       800-SET-SS002-SMEI-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    TEIKI1P-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    TEIKI1P-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    定期請求収納ファイル読込
      *****************************************************************
       910-TEIKI06-READ-SEC            SECTION.
      *
           READ    TEIKI06-FILE
           AT  END
               MOVE    1               TO  FLG-TEIKI06
           END-READ
      *
           IF    ( FLG-TEIKI06         =   ZERO )
               COMPUTE CNT-TEIKI06     =   CNT-TEIKI06 +   1
           END-IF
      *
           .
       910-TEIKI06-READ-EXT.
           EXIT.
      *****************************************************************
      *    請求取消対象収納ファイル読込
      *****************************************************************
       910-TEIKI07-READ-SEC            SECTION.
      *
           READ    TEIKI07-FILE
           AT  END
               MOVE    1               TO  FLG-TEIKI07
           END-READ
      *
           .
       910-TEIKI07-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    単独更新用日別収納ファイル読込
      *****************************************************************
       910-TEIKI08-READ-SEC            SECTION.
      *
           READ    TEIKI08-FILE
           AT  END
               MOVE    1               TO  FLG-TEIKI08
           END-READ
      *
           IF    ( FLG-TEIKI08         =   ZERO )
               COMPUTE CNT-TEIKI08     =   CNT-TEIKI08 +   1
           END-IF
      *
           .
       910-TEIKI08-READ-EXT.
           EXIT.
      *****************************************************************
      *    定期請求収納ファイル読込
      *****************************************************************
       910-TEIKI09-READ-SEC            SECTION.
      *
           READ    TEIKI09-FILE
           AT  END
               MOVE    1               TO  FLG-TEIKI09
           END-READ
      *
           IF    ( FLG-TEIKI09         =   ZERO )
               COMPUTE CNT-TEIKI09     =   CNT-TEIKI09 +   1
           END-IF
      *
           .
       910-TEIKI09-READ-EXT.
           EXIT.
      *****************************************************************
      *    定期請求管理情報削除処理
      *****************************************************************
       900-TEIKIKANRI-DEL-SEC          SECTION.
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKIKANRI-HOSPNUM
           MOVE    TEIKIKANRI-REC      TO  MCPDATA-REC
           MOVE    "tbl_teikikanri"    TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF  (   MCP-RC          NOT =   ZERO )
               MOVE    "0002"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       900-TEIKIKANRI-DEL-EXT.
            EXIT.
      *****************************************************************
      *    定期請求印刷情報削除処理
      *****************************************************************
       900-TEIKIPRT-DEL-SEC            SECTION.
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKIPRT-HOSPNUM
           MOVE    TEIKIPRT-REC        TO  MCPDATA-REC
           MOVE    "tbl_teikiprt"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF  (   MCP-RC          NOT =   ZERO )
               MOVE    "0003"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       900-TEIKIPRT-DEL-EXT.
            EXIT.
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               MOVE    1               TO  FLG-PTINF
      *        INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       900-PTNYUINRRK-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求履歴情報読込
      *****************************************************************
       900-TEIKIRRK-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TEIKIRRK
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKIRRK-HOSPNUM
           MOVE    TEIKIRRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_teikirrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-TEIKIRRK
               MOVE   MCPDATA-REC      TO  TEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-TEIKIRRK
           END-IF
      *
           MOVE    "tbl_teikirrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TEIKIRRK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者請求履歴情報読込
      *****************************************************************
       900-PTTEIKIRRK-KEY6-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE   MCPDATA-REC      TO  PTTEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTTEIKIRRK
               INITIALIZE                  PTTEIKIRRK-REC
           END-IF
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTTEIKIRRK-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納明細検索処理(KEY7)
      *****************************************************************
       900-SYUMEI-KEY7-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
      *    伝票番号枝番を求める
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
                MOVE    MCPDATA-REC    TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE     1              TO  FLG-SYUMEI
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUMEI-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納明細検索処理(KEY2)
      *****************************************************************
       900-SYUMEI-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納明細FETCH処理(KEY2)
      *****************************************************************
       900-SYUMEI-KEY2-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    日別収納更新処理
      *****************************************************************
       900-SYUDAY-UPD-SEC              SECTION.	
      *
           MOVE    WKSYUDAY-REC        TO  SYUDAY-REC
           PERFORM 400-SYUDAY-CHK-SEC
           IF    ( FLG-SYUDAY          =   ZERO )
      *
      *        既に存在する場合はUPDATE
               MOVE    SDAY-CREYMD     TO  WRK-CREYMD
               MOVE    WKSYUDAY-REC    TO  SYUDAY-REC
      *
               MOVE    WRK-CREYMD      TO  SDAY-CREYMD
               MOVE    TEIKI1P-JOBYMD  TO  SDAY-UPYMD
               MOVE    TEIKI1P-JOBHMS  TO  SDAY-UPHMS
               MOVE    SYUDAY-REC      TO  MCPDATA-REC
               MOVE    "tbl_syuday"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF  (   MCP-RC          NOT =   ZERO )
                   MOVE    "0010"      TO  ERR-ERRCD
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           ELSE
      *
      *        存在しない場合はINSERT
               MOVE    WKSYUDAY-REC    TO  SYUDAY-REC
      *
               MOVE    TEIKI1P-JOBYMD  TO  SDAY-UPYMD
                                           SDAY-CREYMD
               MOVE    TEIKI1P-JOBHMS  TO  SDAY-UPHMS
               MOVE    SYUDAY-REC      TO  MCPDATA-REC
               MOVE    "tbl_syuday"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF  (   MCP-RC          NOT =   ZERO )
                   MOVE  "0031"        TO  ERR-ERRCD
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       900-SYUDAY-UPD-EXT.
           EXIT.
      *****************************************************************
      *    日別収納削除処理
      *****************************************************************
       900-SYUDAY-DEL-SEC              SECTION.
      *
           MOVE    WKSYUDAY-REC    TO  MCPDATA-REC
           MOVE    "tbl_syuday"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF  (   MCP-RC          NOT =   ZERO )
               MOVE    "0011"      TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       900-SYUDAY-DEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号マスター読込（主キー）
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    TEIKI1P-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID            TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
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
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    定期管理検索処理(KEY2)
      *****************************************************************
       900-TEIKIKANRI-KEY2-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-TEIKIKANRI
      *
           INITIALIZE                      TEIKIKANRI-REC
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKIKANRI-HOSPNUM
           MOVE    TEIKI1P-JOBYMD      TO  TEIKIKANRI-CREYMD
           MOVE    TEIKI1P-JOBHMS      TO  TEIKIKANRI-CREHMS
           MOVE    TEIKIKANRI-REC      TO  MCPDATA-REC
           MOVE    "tbl_teikikanri"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEIKIKANRI-REC
           ELSE
               MOVE    1               TO  FLG-TEIKIKANRI
               INITIALIZE                  TEIKIKANRI-REC
           END-IF
      *
           MOVE    "tbl_teikikanri"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TEIKIKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター存在チェック
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               MOVE    1           TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
           INITIALIZE                  BUNBEN-REC
      *
           MOVE    SYU-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    SYU-PTID        TO  BUNBEN-PTID
           MOVE    SYU-NYUGAIKBN   TO  BUNBEN-NYUGAIKBN
           MOVE    SYU-NYUIN-RRKNUM
                                   TO  BUNBEN-RRKNUM
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BUNBEN-KEY3-SEL-EXT.
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
      *    ＤＢ検索処理（FHETCHは行わない)
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
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE   LOW-VALUE            TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE   LOW-VALUE            TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *

      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           IF    ( FLG-ERR         =   ZERO )
               MOVE   LOW-VALUE        TO  MCP-TABLE
                                           MCP-PATHNAME
               MOVE    "DBCOMMIT"      TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-IF
      *
           MOVE   LOW-VALUE            TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理
      *****************************************************************
       990-PROGRAM-FINISH-SEC          SECTION.
      *
           STOP   RUN
      *
           .
       990-PROGRAM-FINISH-EXT.
           EXIT.
