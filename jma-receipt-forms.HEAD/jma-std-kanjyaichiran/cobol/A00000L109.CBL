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
       PROGRAM-ID.             A00000L109.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者一覧
      *  コンポーネント名  : (診療行為)患者一覧
      *  管理者            :
      *  作成日付    作業者       記述
      *  04/02/20    @@@@-高橋    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者     日付        内容
      *  01.00.01    @@@-門間   2010/12/07  コメント履歴削除
      *                                     open-cobol1.0対応
      *  01.00.02    @@@-田中   2011/08/03  診療行為の集計方法を修正
      *  01.00.03    @@@-門間   2011/11/07  CSVファイル作成機能追加
      *  01.00.04    @@@-門間   2013/04/08  同患者、同科で医師が異なる場合、
      *                                     先に受診した医師にまとまる不具合の修正
      *  02.00.01    @@@-門間   2014/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.02    @@@-門間   2015/10/09  指定医師以外のレコードが抽出されないように修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *    初診患者抽出一時ファイル
           SELECT  L109T-FILE      ASSIGN  AL109
                                   ORGANIZATION    INDEXED
                                   ACCESS  MODE    DYNAMIC
                                   RECORD  KEY     L109T-KEY
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
      *    初診患者抽出一時ファイル
       FD  L109T-FILE.
           COPY    "A00000L109TMP.INC".
      *
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
           COPY    "A00000L109.INC".
      *
      *    一時ファイル(外部ファイル名称)
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //AL109//.
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
           03  STS-PARA                PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SRYKA-ALL           PIC 9(01).
           03  FLG-DRCD-ALL            PIC 9(01).
           03  FLG-SRYCD-ALL           PIC 9(01).
           03  FLG-PRSORT              PIC X(01).
           03  FLG-GET                 PIC 9(01).
           03  FLG-END                 PIC 9(01).
           03  FLG-ATEND               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SYSKANRI2           PIC 9(01).
           03  FLG-SYSKANRI3           PIC 9(01).
           03  FLG-INPUT               PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-SYUMEI              PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-NYUINACT            PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-PTBYO               PIC 9(01).
           03  FLG-BYO                 PIC 9(01).
           03  FLG-F-JYURRK            PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-SRYKARRK            PIC 9(01).
           03  INP-STS                 PIC 9(04).
           03  FLG-11DUMMY             PIC 9(01).
           03  FLG-12DUMMY             PIC 9(01).
           03  FLG-NYUGAI              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                PIC 9(02).
           03  CNT-PAGE                PIC 9(06).
           03  WRK-PRDTLCNT            PIC 9(05).
           03  CNT-BYO                 PIC 9(05).
           03  CNT-DRCD                PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  SELKA-IDX               PIC 9(04).
           03  IDX1                    PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDX3                    PIC 9(04).
           03  IDX4                    PIC 9(04).
           03  IDX5                    PIC 9(04).
           03  IDX6                    PIC 9(04).
           03  IDX7G.
               05  IDX7                PIC 9(02).
           03  IDX8G.
               05  IDX8                PIC 9(02).
           03  DIDXG.
               05  DIDX                PIC 9(02).
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDX-S                   PIC 9(07).
           03  IDX-N                   PIC 9(07).
           03  IDX-MAX                 PIC 9(07).
           03  JIHIIDX                 PIC 9(04).
           03  NG                      PIC 9(04).
           03  IDX-DAY                 PIC 9(02).
           03  IDX-ST                  PIC 9(03).
           03  IDX-ED                  PIC 9(03).
      *
           03  IDXP              PIC 9(05).
           03  IDX-R                   PIC 9(01).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-PRTYM          PIC X(06).
           03  WRK-PARA-PRTLYMD        PIC X(08).
           03  WRK-PARA-PRTSYMD        PIC X(08).
           03  WRK-PARA-SRYKA          PIC X(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH                PIC X(64).
           03  WRK-RECEERR             PIC X(200).
           03  WRK-PARA-DENPPRTYMDWH   PIC X(22).
           03  WRK-SYSYMDWH            PIC X(22).
           03  WRK-RENNUM              PIC 9(03).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-PTNUM6              PIC 9(06).
           03  WRK-SYUHKNNUM           PIC X(03).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-MONEY               PIC 9(08).
           03  WRK-JIHI                PIC 9(08).
           03  WRK-SYUEKI              PIC 9(08).
           03  WRK-JIHI2               PIC 9(08).
           03  WRK-TJIHI-G             OCCURS  5.
               05  WRK-TJIHI           PIC X(08).
               05  WRK-TJIHI-FLG       PIC 9(01).
           03  WRK-TJIHIN-G            OCCURS  5.
               05  WRK-TJIHIN          PIC X(08).
               05  WRK-TJIHIN-FLG      PIC 9(01).
           03  WRK-4001-TENTANKA       PIC 9(02)V9(01).
           03  WRK-RENNUM-X            PIC ZZ9.
           03  WRK-PAGE                PIC ZZ9.
           03  WRK-Z3                  PIC ZZ9.
           03  WRK-Z5                  PIC ZZZZ9.
           03  WRK-S9-3                PIC --9.
           03  WRK-Z9-5                PIC ZZ,ZZ9.
           03  WRK-Z9                  PIC ZZZZZZZZZZZ9.
           03  WRK-ZZ                  PIC ZZZZZZZZZZZZ.
           03  WRK-S9                  PIC -----------9.
           03  WRK-SZ                  PIC -----------Z.
           03  WRK-NYUGAIKBN           PIC X(01).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKANM             PIC X(30).
           03  WRK-PRDRCD              PIC X(05).
           03  WRK-PRSRYCD             PIC X(18).
           03  WRK-L100LNK-YMD1        PIC X(22).
           03  WRK-L100LNK-YMD2        PIC X(22).
           03  WRK-KOH1FTNNUM          PIC X(30).
           03  WRK-KOH2FTNNUM          PIC X(30).
           03  WRK-KOH3FTNNUM          PIC X(30).
           03  WRK-KIGONUM             PIC X(80).
           03  WRK-NUM                 PIC X(80).
           03  WRK-HEN-KIGONUM         PIC X(80).
           03  WRK-BIRTHDAY.
             05  WRK-BIRTHY            PIC 9(04).
             05  WRK-BIRTHM            PIC 9(02).
             05  WRK-BIRTHD            PIC 9(02).
           03  WRK-F-SRYYMD            PIC X(08).
           03  WRK-FG-SRYYMD           PIC X(08).
           03  WRK-FN-SRYYMD           PIC X(08).
           03  WRK-NENREI              PIC 9(03).
           03  WRK-SRYYMDDMY           PIC 9(10).
      *    文字変換
           03  WRK-ZENKAKU-G.
               05  WRK-ZENKAKU         PIC X(02)  OCCURS 10.
           03  WRK-MOJI                PIC X(10).
           03  WRK-MOJISU              PIC 9(02).
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
           03  WRK-PTID                PIC 9(10).
           03  DEBUG-CNT               PIC 9(03).
      *    SQL用
           03  WRK-SQL                 PIC X(20000).
           03  WRK-SQL-PNT             PIC 9(05).
           03  WRK-SQL-COUNT           PIC X(20000).
           03  WRK-SQLC                PIC 9(05).
      *
           03  WRK-SYS-BEDSU           PIC 9(04).
           03  WRK-SYS-BEDSUIPN        PIC 9(04).
           03  WRK-STARTYMD.
               05  WRK-STARTY          PIC 9(04).
               05  WRK-STARTM          PIC 9(02).
               05  WRK-STARTD          PIC 9(02).
           03  WRK-ENDYMD.
               05  WRK-ENDY            PIC 9(04).
               05  WRK-ENDM            PIC 9(02).
               05  WRK-ENDD            PIC 9(02).
           03  WRK-YM.
               05  WRK-Y               PIC 9(04).
               05  WRK-M               PIC 9(02).
           03  WRK-STARTDAY            PIC 9(02).
           03  WRK-ENDDAY              PIC 9(02).
           03  WRK-OBJYMD              PIC 9(08).
           03  WRK-HKNCOMBI            PIC 9(04).
      *
       01  WRK-SRYKA-AREA.
           03  WRK-SRYKAMAX            PIC 9(03).
           03  WRK-SRYKA-TBL           OCCURS 100.
               05  WRK-SRYKA-CD        PIC X(02).
               05  WRK-SRYKANAME       PIC X(30).
      *
       01  WRK-CONS-AREA.
      *    自費保険番号
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)  VALUE "980".
      *    労災保険番号
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)  VALUE "971".
      *    自賠責保険番号
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)  VALUE "973".
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE           PIC 9(03)    VALUE 100.
           03  WRK-ITEM                PIC X(200)   VALUE SPACE.
           03  WRK-CSVDATA             PIC X(20000) VALUE SPACE.
      *
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-RENNUM        PIC X(50) VALUE "連番".
               07  CSV-HEAD-PTNUM         PIC X(50) VALUE "患者番号".
               07  CSV-HEAD-PTKANANAME    PIC X(50) VALUE "カナ氏名".
               07  CSV-HEAD-PTNAME        PIC X(50) VALUE "患者氏名".
               07  CSV-HEAD-SEX           PIC X(50) VALUE "性別".
               07  CSV-HEAD-BIRTHDAY      PIC X(50) VALUE "生年月日".
               07  CSV-HEAD-AGE           PIC X(50) VALUE "年齢".
               07  CSV-HEAD-HKNJANUM      PIC X(50) VALUE "保険者番号".
               07  CSV-HEAD-HONKZKKBN     PIC X(50) VALUE "本/家".
               07  CSV-HEAD-NYUGAI        PIC X(50) VALUE "外/入".
               07  CSV-HEAD-SRYKA         PIC X(50) VALUE "診療科".
               07  CSV-HEAD-KIGONUM       PIC X(50) VALUE "記号番号".
               07  CSV-HEAD-KOH1FTNNUM    PIC X(50)
                                          VALUE "公費１負担者番号".
               07  CSV-HEAD-SRYYMD        PIC X(50) VALUE "診察日".
               07  CSV-HEAD-LASTYMD       PIC X(50) VALUE "最終来院日".
             05  CSV-HEAD-R               REDEFINES CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM          PIC X(50) OCCURS 15.
             05  CSV-HEAD-MAX             PIC 9(05) VALUE 15.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    自費管理情報
           COPY    "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    労災情報取得
           COPY    "CPSK4001.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスタ
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為情報
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    受診履歴情報(初来院受診履歴用)
       01  JYURRK-F-REC.
           COPY    "CPJYURRK.INC"       REPLACING  //JYURRK-//
                                        BY         //JYURRK-F-//.
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    入院会計マスタ
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    診療会計マスタ２
       01  NYUINACCT2-REC.
           COPY    "CPNYUINACCT.INC"    REPLACING  //NACCT-//
                                        BY         //NACCT2-//.
      *
      *    入院診療行為情報
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    患者入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者病名情報
       01  PTBYO-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    患者最終来院日
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    点数マスタ
      * 01  TNS-TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    病名マスタ
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    患者マスタ（照会用コピー句）
       01  QUERY-PTINF-REC.
           COPY    "CPQPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "A00000L100LNK.INC".
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING       A00000L100-LNK.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM  100-INIT-SEC
      *
           IF  INP-STS  =  ZERO
             PERFORM  200-MAIN-SEC  UNTIL  FLG-END  =  1
           END-IF
      *
           PERFORM  300-END-SEC
      *
           EXIT  PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-SRYKA-AREA
           INITIALIZE                  L109T
           INITIALIZE                  L109
           INITIALIZE                  SPA-AREA
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    L100LNK-HOSPNUM     TO  JOB-HOSPNUM
                                           SPA-HOSPNUM
           MOVE    L100LNK-JOBID       TO  JOB-JOBID
           MOVE    L100LNK-SHELLID     TO  JOB-SHELLID
           MOVE    "A00000L109"        TO  JOB-PGID
           MOVE    "(診療行為)患者一覧"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                       SGETTEMP-AREA
      *
           MOVE    "AL109TP"                TO  AL109-FILE-ID
           MOVE    L100LNK-PRTKANRI-TERMID  TO  AL109-TERMID
           MOVE    L100LNK-HOSPNUM          TO  AL109-HOSPNUM
      *
           MOVE    AL109-BASENAME           TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR                  TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"            USING  SGETTEMP-AREA
      *
           MOVE    SGETTEMP-FULLNAMES (1)   TO  AL109-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)   TO  RECEERR
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    システム日付セット
           MOVE    L100LNK-PRTKANRI-SKYYMD     TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  WRK-SYSYMDWH
           IF  L100LNK-YMD1  =  SPACE
             MOVE    L100LNK-PRTKANRI-SKYYMD   TO  L100LNK-YMD1
           END-IF
           IF  L100LNK-YMD2  =  SPACE
             MOVE    L100LNK-PRTKANRI-SKYYMD   TO  L100LNK-YMD2
           END-IF
      *
      *    対象年月日１編集
           INITIALIZE                    WRK-SYMD
           MOVE    L100LNK-YMD1          TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  WRK-L100LNK-YMD1
      *
      *    対象年月日２編集
           INITIALIZE  WRK-SYMD
           MOVE    L100LNK-YMD2          TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  WRK-L100LNK-YMD2
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE                 TO  SYS-1001-REC
           INITIALIZE                    SYS-1001-REC
           MOVE    "1001"                TO  SYS-1001-KANRICD
           MOVE    "*"                   TO  SYS-1001-KBNCD
           MOVE     L100LNK-PRTKANRI-SKYYMD    TO  SYS-1001-STYUKYMD
                                                   SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM           TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC          TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC         TO  SYS-1001-REC
             MOVE    SYS-1001-HOSPNUM    TO  WRK-PARA-HOSPNUM
             MOVE    SYS-1001-BEDSU      TO  WRK-SYS-BEDSU
             MOVE    SYS-1001-BEDSUIPN   TO  WRK-SYS-BEDSUIPN
           ELSE
               MOVE    "医療機関ＩＤが取得できませんでした。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    診療科情報
           IF  L100LNK-SRYKA  =  SPACE
             MOVE   1        TO  FLG-SRYKA-ALL
           ELSE
             INITIALIZE                  SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    L100LNK-SRYKA       TO  SYS-1005-KBNCD
             MOVE    L100LNK-YMD2        TO  WRK-SYMD
             MOVE    L100LNK-YMD2        TO  SYS-1005-STYUKYMD
             MOVE    L100LNK-YMD1        TO  SYS-1005-EDYUKYMD
             MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             PERFORM  800-SYSKANRI-READ-SEC
             IF  FLG-SYSKANRI  =  ZERO
               MOVE    MCPDATA-REC       TO  SYS-1005-REC
             ELSE
               MOVE    1                 TO  INP-STS
               MOVE    "診療科コードの値が不正です。"
                                         TO  WRK-RECEERR
               PERFORM  500-ERR-HENSYU-SEC
             END-IF
           END-IF
      *
      *    医師情報
           IF  L100LNK-DRCD  =  SPACE
             MOVE   1        TO   FLG-DRCD-ALL
           ELSE
             INITIALIZE                  SYS-1010-REC
             MOVE    "1010"              TO  SYS-1010-KANRICD
             MOVE    L100LNK-DRCD        TO  SYS-1010-KBNCD
             MOVE    L100LNK-YMD2        TO  WRK-SYSYMD
             MOVE    L100LNK-YMD2        TO  SYS-1010-STYUKYMD
             MOVE    L100LNK-YMD1        TO  SYS-1010-EDYUKYMD
             MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
             MOVE    SYS-1010-REC        TO  MCPDATA-REC
             PERFORM 800-SYSKANRI-READ-SEC
             IF   FLG-SYSKANRI  =  ZERO
               MOVE  MCPDATA-REC         TO  SYS-1010-REC
             ELSE
               MOVE  2                   TO  INP-STS
               MOVE    "医師コードの値が不正です。"
                                         TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
             END-IF
           END-IF
      *
      *    診療行為情報
           IF   L100LNK-SELCD  =  SPACE
             MOVE    "診療行為コードが入力されていません。"
                                         TO  WRK-RECEERR
             PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    明細出力順序
           IF  L100LNK-PRSORT  =  SPACE
               OR  L100LNK-PRSORT  =  "0"
               OR  L100LNK-PRSORT  =  "1"
               OR  L100LNK-PRSORT  =  "2"
             MOVE    L100LNK-PRSORT   TO  FLG-PRSORT
           ELSE
             MOVE    3                TO  INP-STS
             MOVE    "印字順序の値が不正です。"   TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    診療行為患者抽出
           PERFORM  320-SRYACT-GET-SEC
      *
      *    診療行為患者印刷
           PERFORM  330-PRT-MAIN-SEC
      *
           MOVE   1      TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面入力値阿エラー処理
      *****************************************************************
       210-ERR-SEC                SECTION.
      *
           INITIALIZE                  L109
      *
           ADD  1                      TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  L109-PAGE
           MOVE    "(診療行為)"        TO  L109-MIDASI
           MOVE    WRK-SYSYMDWH        TO  L109-SYSYMD
           MOVE    WRK-L100LNK-YMD1    TO  L109-YMD1
           MOVE    WRK-L100LNK-YMD2    TO  L109-YMD2
           EVALUATE  INP-STS
             WHEN  1
               MOVE "入力された診療科コードの値が不正です。"
                                       TO  L109-ERRMSG
             WHEN  2
               MOVE "入力された医師コードの値が不正です。"
                                       TO  L109-ERRMSG
             WHEN  3
               MOVE "入力された明細出力順序コードの値が不正です。"
                                       TO  L109-ERRMSG
           END-EVALUATE
           PERFORM  340-PRINT-OUT-SEC
      *
           .
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴 患者情報抽出処理
      *****************************************************************
       320-SRYACT-GET-SEC      SECTION.
      *
           OPEN    OUTPUT         L109T-FILE
           CLOSE                  L109T-FILE
           OPEN    I-O            L109T-FILE
      *
      *    対象月設定
           MOVE    L100LNK-YMD1       TO  WRK-STARTYMD
           MOVE    L100LNK-YMD1(1:6)  TO  WRK-YM
           IF   L100LNK-YMD2  =  SPACE
             MOVE    L100LNK-YMD1     TO  WRK-ENDYMD
           ELSE
             MOVE    L100LNK-YMD2     TO  WRK-ENDYMD
           END-IF
      *
      *    集計範囲編集
           PERFORM  UNTIL  WRK-YM  >  WRK-ENDYMD(1:6)
      *    診療行為
             PERFORM  900-SRYACT-SELECT-SEC
             PERFORM  UNTIL  FLG-SRYACT  =  1
               IF  ( SRY-SRYCD(1)  =  L100LNK-SELCD
                   OR  SRY-SRYCD(2)  =  L100LNK-SELCD
                   OR  SRY-SRYCD(3)  =  L100LNK-SELCD
                   OR  SRY-SRYCD(4)  =  L100LNK-SELCD
                   OR  SRY-SRYCD(5)  =  L100LNK-SELCD )
      *    診療行為情報読み込み
                 PERFORM  900-SRYACCT-READ-SEC
      *
                 MOVE    SRY-SRYKA        TO  WRK-SRYKA
                 MOVE    SRY-PTID         TO  WRK-PTID
                 MOVE    SRY-NYUGAIKBN    TO  WRK-NYUGAIKBN
                 MOVE    ACCT-HKNCOMBI    TO  WRK-HKNCOMBI
                 MOVE    2                TO  FLG-NYUGAI
      *
      *      対象日設定
                 IF  L100LNK-YMD1(1:6)  =  WRK-YM
                   MOVE    WRK-STARTD     TO  WRK-STARTDAY
                 ELSE
                   MOVE    1              TO  WRK-STARTDAY
                 END-IF
                 IF  L100LNK-YMD2(1:6)  =  WRK-YM
                   MOVE    WRK-ENDD       TO  WRK-ENDDAY
                 ELSE
                   MOVE    31             TO  WRK-ENDDAY
                 END-IF
      *    診療会計情報読込み
                 PERFORM  VARYING  IDX-DAY  FROM  WRK-STARTDAY  BY  1
                          UNTIL  IDX-DAY  >   WRK-ENDDAY
      *
                   IF  ACCT-DAY(1 IDX-DAY)  >  ZERO
      *    処理を行う年月日を格納
                     MOVE    WRK-YM       TO  WRK-OBJYMD(1:6)
                     MOVE    IDX-DAY      TO  WRK-OBJYMD(7:2)
      *
                     PERFORM  320-WRITE-TEMP-SEC
                   END-IF
      *
                 END-PERFORM
               END-IF
               PERFORM  900-SRYACT-FETCH-SEC
             END-PERFORM
             PERFORM  900-SRYACT-CLOSE-SEC
      *
      *    入院診療行為情報読み込み
             PERFORM  900-NYUINACT-SELECT-SEC
             PERFORM  UNTIL  FLG-NYUINACT  =  1
      *
               IF  ( NSRY-SRYCD(1)  =  L100LNK-SELCD
                   OR  NSRY-SRYCD(2)  =  L100LNK-SELCD
                   OR  NSRY-SRYCD(3)  =  L100LNK-SELCD
                   OR  NSRY-SRYCD(4)  =  L100LNK-SELCD
                   OR  NSRY-SRYCD(5)  =  L100LNK-SELCD )
      *    入院会計情報読込み
                 PERFORM  900-NYUINACCT-READ-SEC
      *
                 MOVE    NSRY-PTID       TO  WRK-PTID
                 MOVE    "1"             TO  WRK-NYUGAIKBN
                 MOVE    1               TO  FLG-NYUGAI
      *
      *      対象日設定
                 IF  L100LNK-YMD1(1:6)  =  WRK-YM
                   MOVE    WRK-STARTD    TO  WRK-STARTDAY
                 ELSE
                   MOVE    1             TO  WRK-STARTDAY
                 END-IF
                 IF  L100LNK-YMD2(1:6)  =  WRK-YM
                   MOVE    WRK-ENDD      TO  WRK-ENDDAY
                 ELSE
                   MOVE    31            TO  WRK-ENDDAY
                 END-IF
      *
                 PERFORM  VARYING  IDX-DAY  FROM  WRK-STARTDAY  BY  1
                          UNTIL  IDX-DAY  >   WRK-ENDDAY
      *
      *    外泊日に食事のフラグを削除
                   PERFORM  920-NYUINACCT-READ-SEC
      *
                   IF   NACCT-DAY(IDX-DAY)  >  ZERO
      *    処理を行う年月日を格納
                     MOVE    WRK-YM      TO  WRK-OBJYMD(1:6)
                     MOVE    IDX-DAY     TO  WRK-OBJYMD(7:2)
      *
                     PERFORM  320-WRITE-TEMP-SEC
                   END-IF
                 END-PERFORM
      *
               END-IF
               PERFORM  900-NYUINACT-FETCH-SEC
             END-PERFORM
             PERFORM  900-NYUINACT-CLOSE-SEC
      *
      *    集計対象月カウント
             IF   WRK-M  =  12
               ADD   1            TO  WRK-Y
               MOVE    1          TO  WRK-M
             ELSE
               ADD   1            TO  WRK-M
             END-IF
      *
           END-PERFORM
      *
           CLOSE                   L109T-FILE
      *
           .
       320-SRYACT-GET-EXT.
           EXIT.
      *****************************************************************
      *    件数判別処理
      *****************************************************************
       320-WRITE-TEMP-SEC      SECTION.
      *
           INITIALIZE   L109T
      *
      *    診療科・医師・保険組合せ情報
           IF   FLG-DRCD-ALL  =  "1"
             MOVE    SPACE             TO  L109T-DRCD
             IF  FLG-NYUGAI  =  1
               PERFORM  900-PTNYUINRRK-READ-SEC
               MOVE    PTNYUINRRK-HKNCOMBINUM   TO  WRK-HKNCOMBI
               MOVE    PTNYUINRRK-NYUINKA       TO  WRK-SRYKA
             END-IF
           ELSE
             IF  FLG-NYUGAI  =  2
      *
      *        該当医師抽出処理
               PERFORM  900-JYURRK-READ-SEC
               PERFORM  UNTIL  FLG-JYURRK  =  1
      *
                 COMPUTE  IDX-R  =  JYURRK-RENNUM + 1
      *
                 IF  (     JYURRK-SRYYMD            =  WRK-OBJYMD
                       AND ACCT-DAY(IDX-R IDX-DAY)  >  ZERO
                       AND JYURRK-DRCD              =  L100LNK-DRCD )
                   MOVE    JYURRK-DRCD        TO  L109T-DRCD
                 END-IF
      *
                 PERFORM  900-JYURRK-FETCH-SEC
               END-PERFORM
               PERFORM  900-JYURRK-CLOSE-SEC
      *
             ELSE
               PERFORM  900-PTNYUINRRK-READ-SEC
               MOVE    PTNYUINRRK-DRCD1         TO  L109T-DRCD
               MOVE    PTNYUINRRK-HKNCOMBINUM   TO  WRK-HKNCOMBI
               MOVE    PTNYUINRRK-NYUINKA       TO  WRK-SRYKA
             END-IF
           END-IF
      *
      *    患者情報抽出
           INITIALIZE                  PTINF-REC
           MOVE     L100LNK-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
           PERFORM  900-PTINF-READ-SEC
      *
      *    患者番号情報抽出
           INITIALIZE                  PTNUM-REC
           MOVE     L100LNK-HOSPNUM    TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID            TO  PTNUM-PTID
           PERFORM  940-PTNUM-READ-SEC
      *
      *    保険組合せマスター検索
           INITIALIZE                  HKNCOMBI-REC
           MOVE    L100LNK-HOSPNUM     TO  COMB-HOSPNUM
           MOVE    WRK-PTID            TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
           PERFORM  930-HKNCOMBI-READ-SEC
      *
      *    患者保険情報情報抽出
           INITIALIZE                  PTHKNINF-REC
           MOVE    L100LNK-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    WRK-PTID            TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           PERFORM  950-PTHKNINF-READ-SEC
      *
      *    最終来院日
           INITIALIZE                  SRYKARRK-REC
           MOVE    L100LNK-HOSPNUM     TO  KARRK-HOSPNUM
           MOVE    WRK-PTID            TO  KARRK-PTID
           MOVE    WRK-SRYKA           TO  KARRK-SRYKA
           PERFORM  970-SRYKARRK-READ-SEC
      *
      *    公費情報１
           INITIALIZE                  PTKOHINF-REC
           MOVE    L100LNK-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    WRK-PTID            TO  PTKOH-PTID
           MOVE    COMB-KOH1ID         TO  PTKOH-KOHID
           PERFORM  960-PTKOHINF-READ-SEC
           MOVE    PTKOH-FTNJANUM      TO  WRK-KOH1FTNNUM
      *
           MOVE    WRK-SRYKA           TO  L109T-SRYKA
           MOVE    L100LNK-SELCD       TO  L109T-SRYCD
           MOVE    PTNUM-PTNUM         TO  L109T-PTNUM
           MOVE    PTINF-KANANAME      TO  L109T-KANANAME
           MOVE    PTINF-NAME          TO  L109T-NAME
           MOVE    PTINF-SEX           TO  L109T-SEX
           MOVE    PTINF-BIRTHDAY      TO  L109T-BIRTHDAY
           MOVE    PTHKN-HKNJANUM      TO  L109T-HKNJANUM
           MOVE    PTHKN-HONKZKKBN     TO  L109T-HONKAZKKBN
      *
      *    記号・番号
      *    全角変換
           MOVE    PTHKN-KIGO          TO  WRK-MAE-INPUT
           PERFORM  31012-HENKAN-SEC
           MOVE    WRK-OUT-INPUT       TO  WRK-KIGONUM
           MOVE    PTHKN-NUM           TO  WRK-MAE-INPUT
           PERFORM  31012-HENKAN-SEC
           MOVE    WRK-OUT-INPUT       TO  WRK-NUM
      *
           INITIALIZE              WRK-HEN-KIGONUM
                                   L109T-KIGO
                                   L109T-BANGO
           IF  WRK-KIGONUM  =  SPACE
               AND  WRK-NUM  =  SPACE
             MOVE    SPACE             TO  WRK-HEN-KIGONUM
           ELSE
             STRING  WRK-KIGONUM     DELIMITED  BY  SPACE
                     "・"            DELIMITED  BY  SIZE
                     WRK-NUM         DELIMITED  BY  SPACE
                     INTO            WRK-HEN-KIGONUM
             END-STRING
           END-IF
           MOVE    WRK-HEN-KIGONUM     TO  L109T-KIGONUM
      *    後期高齢者は番号のみ
           IF  COMB-HKNNUM  =  "039"
             MOVE   WRK-NUM            TO  L109T-KIGONUM
           ELSE
      *    記号・番号で１６文字以上は別行に編集
             IF  WRK-HEN-KIGONUM(33:)   NOT =  SPACE
               MOVE    SPACE           TO  L109T-KIGONUM
               MOVE    WRK-KIGONUM     TO  L109T-KIGO
               MOVE    WRK-NUM         TO  L109T-BANGO
             END-IF
           END-IF
           MOVE    WRK-KOH1FTNNUM      TO  L109T-KOH1FTNNUM
           MOVE    WRK-NYUGAIKBN       TO  L109T-NYUGAIKBN
           MOVE    WRK-OBJYMD          TO  L109T-SRYYMD
           MOVE    KARRK-LASTYMD       TO  L109T-LASTYMD
      *
      *    サブキー値の設定
           EVALUATE  FLG-PRSORT
             WHEN  SPACE
               MOVE    SPACE           TO  L109T-SUBKEY1
             WHEN  "0"
               MOVE    PTINF-KANANAME  TO  L109T-SUBKEY1
             WHEN  "1"
               MOVE    PTHKN-HKNJANUM  TO  L109T-SUBKEY1
             WHEN  "2"
               MOVE    L109T-LASTYMD   TO  WRK-SRYYMDDMY
               COMPUTE  WRK-SRYYMDDMY  =  9999999999  -  WRK-SRYYMDDMY
               MOVE    WRK-SRYYMDDMY   TO  L109T-SUBKEY1
           END-EVALUATE
      *
      *    診療科・医師判定
           IF  ( L109T-SRYKA  =  L100LNK-SRYKA
                AND  FLG-SRYKA-ALL  =  ZERO )
                OR  FLG-SRYKA-ALL  =  "1" 
              IF   (L109T-DRCD  =  L100LNK-DRCD 
                  AND  FLG-DRCD-ALL  =  ZERO )
                  OR  FLG-DRCD-ALL  =  "1" 
             WRITE  L109T
             END-IF
           END-IF
      *
           .
       320-WRITE-TEMP-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索患者印刷メイン処理
      *****************************************************************
       330-PRT-MAIN-SEC                SECTION.
      *
           INITIALIZE      L109T
           OPEN    INPUT   L109T-FILE
      *
      *    患者情報読込
           PERFORM  900-L109T-READ-SEC
           MOVE    L109T-SRYCD     TO  WRK-PRSRYCD
           MOVE    L109T-DRCD      TO  WRK-PRDRCD
      *
      *    CSVヘッダ編集処理
           PERFORM  800-CSV-HEAD-SEC
      *
           PERFORM  VARYING  IDX2  FROM  1  BY  1
                    UNTIL  FLG-ATEND  =  1
      *
      *    明細処理
             PERFORM  330-PRT-DETL-SEC
      *
      *    患者情報読込
             PERFORM  900-L109T-READ-SEC
           END-PERFORM
      *
      *    最終頁の印刷
           IF  IDX2  >  15
             PERFORM  330-PRT-HEAD-SEC
             PERFORM  340-PRINT-OUT-SEC
             PERFORM  230-TOUKEICSV-SEC
             INITIALIZE               L109
           END-IF
           IF  FLG-DRCD-ALL  =  "2"
             MOVE    "＜医師別計＞"    TO  L109-CNTTIT2
             MOVE    CNT-DRCD          TO  WRK-Z5
             MOVE    WRK-Z5            TO  L109-CNT2(1:5)
             MOVE    "件"              TO  L109-CNT2(8:2)
           END-IF
           MOVE    "＜＜総合計＞＞"    TO  L109-CNTTIT3
           MOVE    WRK-PRDTLCNT        TO  WRK-Z5
           MOVE    WRK-Z5              TO  L109-CNT3(1:5)
           MOVE    "件"                TO  L109-CNT3(8:2)
      *
           PERFORM  330-PRT-HEAD-SEC
           PERFORM  340-PRINT-OUT-SEC
      *
           PERFORM  400-CSV-GOUKEI-SEC
           PERFORM  230-TOUKEICSV-SEC
      *
           CLOSE  L109T-FILE
           .
       330-PRT-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    患者診療行為データ明細転送
      *****************************************************************
       330-PRT-DETL-SEC                SECTION.
      *
           IF  WRK-PRSRYCD  NOT =  L109T-SRYCD
             IF  IDX2  >  15
                 PERFORM  330-PRT-HEAD-SEC
                 PERFORM  340-PRINT-OUT-SEC
                 PERFORM  230-TOUKEICSV-SEC
                 INITIALIZE        L109
             END-IF
             IF  FLG-DRCD-ALL  =  "2"
               MOVE    "＜医師別計＞"   TO  L109-CNTTIT1
               MOVE    CNT-DRCD         TO  WRK-Z5
               MOVE    WRK-Z5           TO  L109-CNT1(1:5)
               MOVE    "件"             TO  L109-CNT1(8:2)
             END-IF
             PERFORM  330-PRT-HEAD-SEC
             PERFORM  340-PRINT-OUT-SEC
             PERFORM  230-TOUKEICSV-SEC
             MOVE    1                  TO  IDX2
             MOVE    L109T-SRYCD        TO  WRK-PRSRYCD
             MOVE    L109T-DRCD         TO  WRK-PRDRCD
             INITIALIZE              L109
             INITIALIZE              CNT-DRCD
             INITIALIZE              CNT-BYO
           ELSE 
             IF  WRK-PRDRCD  NOT =  L109T-DRCD
               IF  IDX2  >  15
                 PERFORM  330-PRT-HEAD-SEC
                 PERFORM  340-PRINT-OUT-SEC
                 PERFORM  230-TOUKEICSV-SEC
                 INITIALIZE             L109
               END-IF
               MOVE    "＜医師別計＞"   TO  L109-CNTTIT3
               MOVE    CNT-DRCD         TO  WRK-Z5
               MOVE    WRK-Z5           TO  L109-CNT3(1:5)
               MOVE    "件"             TO  L109-CNT3(8:2)
               PERFORM  330-PRT-HEAD-SEC
               PERFORM  340-PRINT-OUT-SEC
               PERFORM  230-TOUKEICSV-SEC
               MOVE    1                TO   IDX2
               MOVE    L109T-DRCD       TO   WRK-PRDRCD
               INITIALIZE               L109
               INITIALIZE               CNT-DRCD
             ELSE
               IF  IDX2  >  15
                 PERFORM  330-PRT-HEAD-SEC
                 PERFORM  340-PRINT-OUT-SEC
                 PERFORM  230-TOUKEICSV-SEC
                 MOVE    1              TO  IDX2
                 INITIALIZE             L109
               END-IF
              END-IF
           END-IF
      *
      *    件数集計
           ADD  1           TO  WRK-PRDTLCNT
           ADD  1           TO  CNT-BYO
           ADD  1           TO  CNT-DRCD
      *    通番
           MOVE    WRK-PRDTLCNT        TO  WRK-Z5
           MOVE    WRK-Z5              TO  L109-NUM(IDX2)
      *
      *    患者番号
           MOVE    L109T-PTNUM         TO  L109-PTNUM(IDX2)
      *    カナ氏名
           MOVE    L109T-KANANAME      TO  L109-KANANAME(IDX2)
      *    氏名
           MOVE    L109T-NAME          TO  L109-NAME(IDX2)
      *    性別
           EVALUATE  L109T-SEX
             WHEN  "1"
               MOVE    "男"            TO  L109-SEX(IDX2)
             WHEN  "2"
               MOVE    "女"            TO  L109-SEX(IDX2)
           END-EVALUATE
      *    生年月日
           MOVE    L109T-BIRTHDAY(1:4)   TO  L109-BIRTHDAY(IDX2)(1:4)
           MOVE    "/"                   TO  L109-BIRTHDAY(IDX2)(5:1)
           MOVE    L109T-BIRTHDAY(5:2)   TO  L109-BIRTHDAY(IDX2)(6:2)
           MOVE    "/"                   TO  L109-BIRTHDAY(IDX2)(8:1)
           MOVE    L109T-BIRTHDAY(7:2)   TO  L109-BIRTHDAY(IDX2)(9:2)
      *    年齢
           MOVE    L100LNK-PRTKANRI-SKYYMD   TO  WRK-SYSYMD
           MOVE    L109T-BIRTHDAY        TO  WRK-BIRTHDAY
           COMPUTE  WRK-NENREI  =  WRK-SYSYY  -  WRK-BIRTHY
           IF  WRK-SYSMM  <  WRK-BIRTHM
             COMPUTE  WRK-NENREI  =  WRK-NENREI  -  1
           END-IF
           MOVE    WRK-NENREI          TO  WRK-Z3
           MOVE    WRK-Z3              TO  L109-AGE(IDX2)
      *    保険者番号
           MOVE    L109T-HKNJANUM      TO  L109-HKNJANUM(IDX2)
      *    本人家族区分
           EVALUATE  L109T-HONKAZKKBN
             WHEN  1
               MOVE    "本"            TO  L109-HONKAZKKBN(IDX2)
             WHEN  2
               MOVE    "家"            TO  L109-HONKAZKKBN(IDX2)
           END-EVALUATE
      *    記号・番号
           MOVE    L109T-KIGONUM       TO  L109-KIGONUM(IDX2)
           MOVE    L109T-KIGO          TO  L109-KIGO(IDX2)
           MOVE    L109T-BANGO         TO  L109-BANGO(IDX2)
      *    公費番号
           MOVE    L109T-KOH1FTNNUM    TO  L109-KOH1FTNNUM(IDX2)
      *    入院外来区分
           EVALUATE L109T-NYUGAIKBN
             WHEN  1
               MOVE    "入"            TO  L109-NYUGAIKBN(IDX2)
             WHEN  2
               MOVE    "外"            TO  L109-NYUGAIKBN(IDX2)
           END-EVALUATE
      *
      *    診療科情報
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    L109T-SRYKA         TO  SYS-1005-KBNCD
           MOVE    L100LNK-YMD2        TO  WRK-SYMD
           MOVE    L100LNK-YMD2        TO  SYS-1005-STYUKYMD
           MOVE    L100LNK-YMD1        TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM  800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC       TO   SYS-1005-REC
             MOVE    SYS-1005-SRYKANAME   TO   L109-SRYKA(IDX2)
           END-IF
      *    診察開始日
           MOVE    L109T-SRYYMD(1:4)   TO  L109-SRYYMD(IDX2)(1:4)
           MOVE    "/"                 TO  L109-SRYYMD(IDX2)(5:1)
           MOVE    L109T-SRYYMD(5:2)   TO  L109-SRYYMD(IDX2)(6:2)
           MOVE    "/"                 TO  L109-SRYYMD(IDX2)(8:1)
           MOVE    L109T-SRYYMD(7:2)   TO  L109-SRYYMD(IDX2)(9:2)
      *    最終来院日
           MOVE    L109T-LASTYMD(1:4)  TO  L109-LASTYMD(IDX2)(1:4)
           MOVE    "/"                 TO  L109-LASTYMD(IDX2)(5:1)
           MOVE    L109T-LASTYMD(5:2)  TO  L109-LASTYMD(IDX2)(6:2)
           MOVE    "/"                 TO  L109-LASTYMD(IDX2)(8:1)
           MOVE    L109T-LASTYMD(7:2)  TO  L109-LASTYMD(IDX2)(9:2)
      *
           PERFORM  400-CSV-DATA-SEC
      *
           .
       330-PRT-DETL-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者診療行為データ明細転送
      *****************************************************************
       330-PRT-HEAD-SEC                SECTION.
      *
           ADD  1         TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  L109-PAGE
           MOVE    "(診療行為)"        TO  L109-MIDASI
           MOVE    WRK-SYSYMDWH        TO  L109-SYSYMD
           MOVE    WRK-L100LNK-YMD1    TO  L109-YMD1
           MOVE    WRK-L100LNK-YMD2    TO  L109-YMD2
      *
      *    診療行為情報
           MOVE    L100LNK-SELCD       TO  L109-SRYCD
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    L100LNK-SELCD       TO  TNS-SRYCD
           MOVE    L100LNK-YMD1        TO  TNS-YUKOSTYMD
           MOVE    L100LNK-YMD2        TO  TNS-YUKOEDYMD
           PERFORM  980-TENSU-READ-SEC
           IF  FLG-BYO  =  ZERO
             MOVE    TNS-NAME          TO  L109-SRYMEI
           END-IF
      *
      *    職員情報
           IF   FLG-DRCD-ALL  NOT =  "1"
             INITIALIZE                SYS-1010-REC
             MOVE    "1010"            TO  SYS-1010-KANRICD
             IF  WRK-PRDRCD  NOT =  SPACE
               MOVE    WRK-PRDRCD      TO  SYS-1010-KBNCD
             ELSE
               MOVE    L100LNK-DRCD    TO  SYS-1010-KBNCD
             END-IF
             MOVE    L100LNK-YMD2      TO  WRK-SYSYMD
             MOVE    L100LNK-YMD2      TO  SYS-1010-STYUKYMD
             MOVE    L100LNK-YMD1      TO  SYS-1010-EDYUKYMD
             MOVE    SPA-HOSPNUM       TO  SYS-1010-HOSPNUM
             MOVE    SYS-1010-REC      TO  MCPDATA-REC
             PERFORM  800-SYSKANRI-READ-SEC
             IF  FLG-SYSKANRI  =  ZERO
               MOVE    MCPDATA-REC     TO  SYS-1010-REC
               MOVE    SYS-1010-NAME   TO  L109-DRNAME
             ELSE
               MOVE    "医師未登録"    TO  L109-DRNAME
             END-IF
           ELSE
             MOVE    "全医師"          TO  L109-DRNAME
           END-IF
           .
       330-PRT-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       340-PRINT-OUT-SEC         SECTION.
      *
           INITIALIZE                           ORCSPRTAREA
           MOVE    "INS"                        TO  SPRT-MODE
           MOVE    L100LNK-PRTKANRI-RENNUM      TO  SPRT-RENNUM
           MOVE    L100LNK-PRTKANRI-TBL-KEY     TO  SPRT-TBL-KEY
           MOVE    L100LNK-PRTKANRI-TBL-GROUP   TO  SPRT-TBL-GROUP
           MOVE    L100LNK-PRTKANRI-SRYYM       TO  SPRT-SRYYM
           MOVE    L100LNK-PRTKANRI-SKYYMD      TO  SPRT-SKYYMD
           MOVE    L100LNK-PRTKANRI-SHELLID     TO  SPRT-SHELLID
           MOVE    L100LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    L100LNK-PRTKANRI-PRIORITY    TO  SPRT-PRIORITY
           MOVE    "A00000L109.red"             TO  SPRT-PRTID
           MOVE    "(診療行為)患者一覧"         TO  SPRT-TITLE
           MOVE    L109                         TO  SPRT-PRTDATA
           MOVE    L100LNK-PRTKANRI-TERMID      TO  SPRT-TERMID
           MOVE    L100LNK-PRTKANRI-OPID        TO  SPRT-OPID
           MOVE    L100LNK-PRTKANRI-PRTNM       TO  SPRT-PRTNM
           CALL    "ORCSPRT"                    USING
                                                ORCSPRTAREA
                                                SPA-AREA
           IF  SPRT-RETURN  =  ZERO
             CONTINUE               ELSE
             MOVE    "印刷ＤＢに更新できませんでした"
                                 TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
           .
       340-PRINT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
           MOVE    1                 TO   IDXP
      *    開始日、終了日
           MOVE  WRK-L100LNK-YMD1    TO   WRK-ITEM(1:22)
           MOVE  "  〜  "            TO   WRK-ITEM(23:6)
           MOVE  WRK-L100LNK-YMD2    TO   WRK-ITEM(29:22)
      *
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    病名コード
           INITIALIZE  WRK-ITEM
           MOVE  "診療行為コード："  TO   WRK-ITEM(1:16)
           MOVE  L100LNK-SELCD       TO   WRK-ITEM(17:9)
      *
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    病名
           INITIALIZE                WRK-ITEM
                                     TNS-TENSU-REC
           MOVE    SPA-HOSPNUM       TO  TNS-HOSPNUM
           MOVE    L100LNK-SELCD     TO  TNS-SRYCD
           MOVE    L100LNK-YMD1      TO  TNS-YUKOSTYMD
           MOVE    L100LNK-YMD2      TO  TNS-YUKOEDYMD
           PERFORM  980-TENSU-READ-SEC
           IF  FLG-BYO  =  ZERO
             MOVE    TNS-NAME          TO  L109-SRYMEI
           END-IF
      *
           MOVE  "診療行為名："      TO   WRK-ITEM(1:12)
           MOVE  L109-SRYMEI         TO   WRK-ITEM(13:100)
      *
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    医師情報
           INITIALIZE                WRK-ITEM
           MOVE  "医師名："          TO   WRK-ITEM(1:8)
      *
           IF    FLG-DRCD-ALL        NOT  =   "1"
             INITIALIZE                    SYS-1010-REC
             MOVE    "1010"            TO  SYS-1010-KANRICD
             IF      WRK-PRDRCD      NOT = SPACE 
               MOVE    WRK-PRDRCD        TO  SYS-1010-KBNCD
             ELSE 
               MOVE    L100LNK-DRCD      TO  SYS-1010-KBNCD
             END-IF 
             MOVE    L100LNK-YMD2      TO  SYS-1010-STYUKYMD
             MOVE    L100LNK-YMD1      TO  SYS-1010-EDYUKYMD
             MOVE    SPA-HOSPNUM       TO  SYS-1010-HOSPNUM
             MOVE    L100LNK-YMD2      TO  WRK-SYSYMD
             MOVE    SYS-1010-REC      TO  MCPDATA-REC
             PERFORM 800-SYSKANRI-READ-SEC
             IF      FLG-SYSKANRI      =    ZERO
               MOVE  MCPDATA-REC         TO  SYS-1010-REC
               MOVE  SYS-1010-NAME       TO  WRK-ITEM(9:80)
             ELSE
               MOVE  "医師未登録"        TO  WRK-ITEM(9:10)
             END-IF
           ELSE
             MOVE  "全医師"          TO  WRK-ITEM(9:6)
           END-IF
      *
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    システム日付
           MOVE  WRK-SYSYMDWH      TO  WRK-ITEM
      *
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    項目名称セット
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL  IDX4  >  CSV-HEAD-MAX
             MOVE    CSV-HEAD-ITEM(IDX4)     TO  WRK-ITEM
             IF   IDX4  >=  CSV-HEAD-MAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
      *
           .
      *
       800-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSV編集処理
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
      *    通番
           MOVE    L109-NUM(IDX2)          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者番号
           MOVE    L109-PTNUM(IDX2)        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    カナ氏名
           MOVE    L109-KANANAME(IDX2)     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    氏名
           MOVE    L109-NAME(IDX2)         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    性別
           MOVE    L109-SEX(IDX2)          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    生年月日
           MOVE    L109-BIRTHDAY(IDX2)     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    年齢
           MOVE    L109-AGE(IDX2)          TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    保険者番号
           MOVE    L109-HKNJANUM(IDX2)     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    本人家族区分
           MOVE    L109-HONKAZKKBN(IDX2)   TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    外来／入院
           MOVE    L109-NYUGAIKBN(IDX2)    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    診療科
           MOVE    L109-SRYKA(IDX2)        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    記号・番号
           IF  L109-KIGONUM(IDX2)  =  SPACE
             IF     L109-KIGO(IDX2)   =  SPACE
               AND  L109-BANGO(IDX2)  =  SPACE
               MOVE    SPACE                   TO  WRK-ITEM
             ELSE
               STRING  L109-KIGO(IDX2)   DELIMITED  BY  SPACE
                       "・"              DELIMITED  BY  SIZE
                       L109-BANGO(IDX2)  DELIMITED  BY  SPACE
                       INTO              WRK-ITEM
               END-STRING
             END-IF
           ELSE
             MOVE    L109-KIGONUM(IDX2)      TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *    公費番号
           MOVE    L109-KOH1FTNNUM(IDX2)   TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    診察日
           MOVE    L109-SRYYMD(IDX2)       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *    最終来院日
           MOVE    L109-LASTYMD(IDX2)      TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           .
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSV合計編集処理
      *****************************************************************
       400-CSV-GOUKEI-SEC            SECTION.
      *
      *    総合計件数
           MOVE    "＜＜＜総合計＞＞＞"    TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
           MOVE    WRK-Z5                  TO  WRK-ITEM
           MOVE    "件"                    TO  WRK-ITEM(6:2)
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           .
       400-CSV-GOUKEI-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-DELIMITER-SEC
           .
      *
       801-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       810-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 820-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-CR-SEC
           .
      *
       810-CSVDATA-EDIT-EXE.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       820-CSVDATA-EDIT-SEC            SECTION.
      *
           IF   WRK-ITEM  NOT =  SPACE
             PERFORM  800-GET-ITEM-SIZE-SEC
             STRING  WRK-ITEM(IDX-ST:IDX-ED)  DELIMITED   BY  SIZE
                                              INTO  WRK-CSVDATA
                                              WITH  POINTER  IDXP
             END-STRING
           END-IF
           .
      *
       820-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    区切り文字編集処理
      *****************************************************************
       800-PUT-DELIMITER-SEC           SECTION.
      *
           STRING    ","       DELIMITED  BY  SIZE
                               INTO  WRK-CSVDATA
                               WITH  POINTER  IDXP
           END-STRING
      *
           .
       800-PUT-DELIMITER-EXT.
           EXIT.
      *****************************************************************
      *    改行文字編集処理
      *****************************************************************
       800-PUT-CR-SEC                  SECTION.
      *
           STRING    X"0D"     DELIMITED  BY  SIZE
                               INTO  WRK-CSVDATA
                               WITH  POINTER  IDXP
           END-STRING
      *
           .
       800-PUT-CR-EXT.
           EXIT.
      *****************************************************************
      *    項目サイズ取得処理
      *****************************************************************
       800-GET-ITEM-SIZE-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX-ST
                                       IDX-ED
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL  IDX1  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX1:)    =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX1:1)  NOT =  SPACE
                 MOVE    IDX1      TO  IDX-ST
                 MOVE    1         TO  IDX-ED
               END-IF
             ELSE
               COMPUTE  IDX-ED  =  IDX-ED  +  1
             END-IF
      *
           END-PERFORM
      *
           .
       800-GET-ITEM-SIZE-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ出力処理
      *****************************************************************
       230-TOUKEICSV-SEC               SECTION.
      *
           INITIALIZE                         ORCSTOUKEICSVAREA
           MOVE    "INS"                      TO  STOUKEICSV-MODE
           MOVE    L100LNK-PRTKANRI-TBL-KEY   TO  STOUKEICSV-TBL-KEY
           MOVE    L100LNK-PRTKANRI-RENNUM    TO  STOUKEICSV-RENNUM
           MOVE    L100LNK-PRTKANRI-TBL-GROUP TO  STOUKEICSV-TBL-GROUP
           MOVE    L100LNK-PRTKANRI-SHORI-RENNUM
                                            TO  STOUKEICSV-SHORI-RENNUM
           MOVE    L100LNK-PRTKANRI-SHELLID   TO  STOUKEICSV-SHELLID
           MOVE    L100LNK-PRTKANRI-SRYYM     TO  STOUKEICSV-SRYYM
           MOVE    ZERO                       TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"                TO  STOUKEICSV-TO-FOLDER
      *
           STRING  SPA-HOSPNUM                DELIMITED  BY  SIZE
                   "shinryoukoui_kanjyaichiran_"
                                              DELIMITED  BY  SIZE
                   L100LNK-YMD1               DELIMITED  BY  SIZE
                   "_"                        DELIMITED  BY  SIZE
                   L100LNK-YMD2               DELIMITED  BY  SIZE
                   ".csv"                     DELIMITED  BY  SIZE
                                              INTO  STOUKEICSV-TO-DATA
           END-STRING
           MOVE    "2"                        TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA                TO  STOUKEICSV-CSVDATA
           MOVE    "(診療行為)患者一覧表"     TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"            USING
                                              ORCSTOUKEICSVAREA
                                              SPA-AREA
           IF  ( STOUKEICSV-RETURN  =  ZERO )
             CONTINUE
           ELSE
             MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                                TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE                         WRK-CSVDATA
           MOVE    1                          TO   IDXP
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF  STS-RECEERR  =  ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT             RECEERR-FILE
      *
             MOVE    WRK-RECEERR        TO  RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ管理開始処理
             MOVE    "JBE"              TO  SJOBKANRI-MODE
             INITIALIZE                 JOBKANRI-REC
             MOVE    SPA-HOSPNUM        TO  JOB-HOSPNUM
             MOVE    L100LNK-JOBID      TO  JOB-JOBID
             MOVE    L100LNK-SHELLID    TO  JOB-SHELLID
             MOVE    WRK-RECEERR        TO  JOB-YOBI
             MOVE    "9999"             TO  JOB-ERRCD
             CALL    "ORCSJOB"          USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
                                        SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    L100LNK-JOBID       TO  JOB-JOBID
           MOVE    L100LNK-SHELLID     TO  JOB-SHELLID
           MOVE    CNT-PAGE            TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT  WRK-HENYMDG  REPLACING  ALL  "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       31012-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF  KANACHK-RC  =  ZERO
             MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-OUT-INPUT
           ELSE
             MOVE    WRK-MAE-INPUT     TO  WRK-OUT-INPUT
           END-IF
      *
           .
       31012-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受信履歴情報検索処理
      *****************************************************************
       900-JYURRK-READ-SEC          SECTION.
      *
           INITIALIZE                   JYURRK-REC
           MOVE    ACCT-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    ACCT-PTID            TO  JYURRK-PTID
           MOVE    ACCT-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    ACCT-SRYKA           TO  JYURRK-SRYKA
           MOVE    ACCT-HKNCOMBI        TO  JYURRK-HKNCOMBINUM
           MOVE    ACCT-ZAINUM          TO  JYURRK-ZAINUM1
                                            JYURRK-ZAINUM2
                                            JYURRK-ZAINUM3
                                            JYURRK-ZAINUM4
                                            JYURRK-ZAINUM5
                                            JYURRK-ZAINUM6
                                            JYURRK-ZAINUM7
                                            JYURRK-ZAINUM8
                                            JYURRK-ZAINUM9
                                            JYURRK-ZAINUM10
                                            JYURRK-ZAINUM11
                                            JYURRK-ZAINUM12
                                            JYURRK-ZAINUM13
                                            JYURRK-ZAINUM14
                                            JYURRK-ZAINUM15
           MOVE    JYURRK-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key20"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
               MOVE    ZERO             TO  FLG-JYURRK
           ELSE
             MOVE    1                  TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受信履歴情報読み込み処理
      *****************************************************************
       900-JYURRK-FETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key20"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    MCPDATA-REC        TO  JYURRK-REC
             MOVE    ZERO               TO  FLG-JYURRK
           ELSE
             MOVE    1                  TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    受信履歴情報クローズ処理
      *****************************************************************
       900-JYURRK-CLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key20"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-JYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DB-SELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DB-READ-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DB-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DB-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    診療会計情報読込処理
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           INITIALIZE                   SRYACCT-REC
           MOVE    SRY-HOSPNUM          TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN        TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID             TO  ACCT-PTID
           MOVE    SRY-SRYKA            TO  ACCT-SRYKA
           MOVE    SRY-SRYYM            TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN           TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM           TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_sryacct"      TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  SRYACCT-REC
               MOVE    ZERO             TO  FLG-SRYACCT
             ELSE
               MOVE    1                TO  FLG-SRYACCT
             END-IF
           ELSE
             MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_sryacct"        TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       900-SRYACT-SELECT-SEC               SECTION.
      *
           INITIALIZE                   SRYACT-REC
           MOVE    WRK-PARA-HOSPNUM     TO  SRY-HOSPNUM
           MOVE    WRK-YM               TO  SRY-SRYYM
           MOVE    SRYACT-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "st_key4"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    ZERO               TO  FLG-SRYACT
           ELSE
             MOVE    1                  TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報FETCH
      *****************************************************************
       900-SRYACT-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "st_key4"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF   MCP-RC  =  ZERO
             MOVE    ZERO               TO  FLG-SRYACT
             MOVE    MCPDATA-REC        TO  SRYACT-REC
           ELSE
             MOVE    1                  TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報 ＤＢ　クローズ処理
      *****************************************************************
       900-SRYACT-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "st_key4"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為情報検索
      *****************************************************************
       900-NYUINACT-SELECT-SEC          SECTION.
      *
           INITIALIZE                   NYUINACT-REC
           MOVE    WRK-PARA-HOSPNUM     TO  NSRY-HOSPNUM
           MOVE    WRK-YM               TO  NSRY-SRYYM
           MOVE    NYUINACT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "st_key1"            TO  MCP-PATHNAME
           MOVE    "tbl_nyuinact"       TO  MCP-TABLE
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             PERFORM  900-NYUINACT-FETCH-SEC
           ELSE
             MOVE    1                  TO  FLG-NYUINACT
           END-IF
           .
       900-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為情報検索
      *****************************************************************
       900-NYUINACT-FETCH-SEC          SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
           MOVE    "tbl_nyuinact"       TO  MCP-TABLE
           MOVE    "st_key1"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    MCPDATA-REC        TO  NYUINACT-REC
             MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
             MOVE    1                  TO  FLG-NYUINACT
           END-IF
           .
       900-NYUINACT-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療行為情報検索
      *****************************************************************
       900-NYUINACT-CLOSE-SEC          SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_nyuinact"       TO  MCP-TABLE
           MOVE    "st_key1"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-NYUINACT-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計情報検索
      *****************************************************************
       900-NYUINACCT-READ-SEC          SECTION.
      *
           INITIALIZE                   NYUINACCT-REC
           MOVE    NSRY-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    NSRY-NYUGAIKBN       TO  NACCT-NYUGAIKBN
           MOVE    NSRY-PTID            TO  NACCT-PTID
           MOVE    NSRY-SRYKA           TO  NACCT-SRYKA
           MOVE    NSRY-SRYYM           TO  NACCT-SRYYM
           MOVE    NSRY-SRYKBN          TO  NACCT-SRYKBN
           MOVE    NSRY-ZAINUM          TO  NACCT-ZAINUM
           MOVE    NYUINACCT-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
             MOVE    "key"              TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  NYUINACCT-REC
               MOVE    ZERO             TO  FLG-NYUINACCT
             ELSE
               MOVE    1                TO  FLG-NYUINACCT
             END-IF
           ELSE
             MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴SELECT
      *****************************************************************
       900-PTNYUINRRK-READ-SEC         SECTION.
      *
           INITIALIZE                   PTNYUINRRK-REC
           MOVE    WRK-PARA-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
           MOVE    WRK-OBJYMD           TO  PTNYUINRRK-NYUINYMD
           MOVE    WRK-OBJYMD           TO  PTNYUINRRK-TAIINYMD
           MOVE    NSRY-PTID            TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key11"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
             MOVE    "key11"            TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  PTNYUINRRK-REC
               MOVE    ZERO             TO  FLG-PTNYUINRRK
             ELSE
               MOVE    1                TO  FLG-PTNYUINRRK
             END-IF
           ELSE
             MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key11"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTNYUINRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院診療会計情報検索
      *****************************************************************
       920-NYUINACCT-READ-SEC           SECTION.
      *
           INITIALIZE                   NYUINACCT2-REC
           MOVE    NACCT-HOSPNUM        TO  NACCT2-HOSPNUM
           MOVE    NACCT-PTID           TO  NACCT2-PTID
           MOVE    NACCT-SRYYM          TO  NACCT2-SRYYM
           MOVE    "2"                  TO  NACCT2-ZAISKBKBN
           MOVE    NYUINACCT2-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key17"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"          TO  MCP-FUNC
             MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
             MOVE    "key17"            TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC      TO  NYUINACCT2-REC
             ELSE
               INITIALIZE               NYUINACCT2-REC
             END-IF
      *
             EVALUATE  NACCT2-DAY(IDX-DAY)
               WHEN  1
               WHEN  2
               WHEN  3
                 IF  NACCT-ZAISKBKBN  =  "4"  OR  "6"
                   MOVE    0            TO  NACCT-DAY(IDX-DAY)
                 END-IF
             END-EVALUATE
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
           MOVE    "key17"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-NYUINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    初診患者データ一時ファイルREAD
      *****************************************************************
       900-L109T-READ-SEC                SECTION.
      *
           READ  L109T-FILE
             NEXT  AT  END
               MOVE    1       TO  FLG-ATEND
           END-READ
      *
           .
       900-L109T-READ-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_syskanri"    TO  MCP-TABLE
             MOVE    "key10"           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
             ELSE
               MOVE    1               TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                 TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み(自費情報)
      *****************************************************************
       810-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_syskanri"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI2
             ELSE
               MOVE    1               TO  FLG-SYSKANRI2
             END-IF
           ELSE
             MOVE    1                 TO  FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    WRK-SYSYMD            TO  ORC-DBYMD
           MOVE    SYS-1005-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC  =  ZERO
             PERFORM  820-SYSKANRI-DBFETCH-SEC
           ELSE
             MOVE    1                   TO  FLG-SYSKANRI3
             PERFORM  820-SYSKANRI-DBCLOSE-SEC
           END-IF
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"             TO  MCP-FUNC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    ZERO                TO  FLG-SYSKANRI3
             MOVE    MCPDATA-REC         TO  SYS-1005-REC
           ELSE
             MOVE    1                   TO  FLG-SYSKANRI3
             PERFORM  820-SYSKANRI-DBCLOSE-SEC
           END-IF
           .
       820-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       820-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptinf"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
             ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE              PTINF-REC
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTINF
             INITIALIZE                PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       930-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_hkncombi"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
             ELSE
               MOVE    1               TO  FLG-HKNCOMBI
             END-IF
           ELSE
             MOVE    1                 TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       930-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者番号情報読み込み
      *****************************************************************
       940-PTNUM-READ-SEC         SECTION.
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptnum"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTNUM-REC
               MOVE    ZERO            TO  FLG-PTNUM
             ELSE
               MOVE    1               TO  FLG-PTNUM
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTNUM
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       940-PTNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       950-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_pthkninf"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
             ELSE
               MOVE    1               TO  FLG-PTHKNINF
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       950-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費マスター読み込み
      *****************************************************************
       960-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
             ELSE
               MOVE    1               TO  FLG-PTKOHINF
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       960-PTKOHINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    最終来院日情報読み込み
      *****************************************************************
       970-SRYKARRK-READ-SEC         SECTION.
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_srykarrk"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  SRYKARRK-REC
               MOVE    ZERO            TO  FLG-SRYKARRK
             ELSE
               MOVE    1               TO  FLG-SRYKARRK
             END-IF
           ELSE
             MOVE    1                 TO  FLG-SRYKARRK
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       970-SRYKARRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為読み込み
      *****************************************************************
       980-TENSU-READ-SEC           SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_tensu"       TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF   MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-BYO
             ELSE
               MOVE    1               TO  FLG-BYO
             END-IF
           ELSE
             MOVE    1                 TO  FLG-BYO
           END-IF
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       980-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBCLOSE-EXT.
           EXIT.
