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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGKT01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 外来まとめ入力
      *  コンポーネント名  : 診療行為入力
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/02/12    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-多々納  09/07/30  リハビリ発症日編集対応
      *  04.05.00    NACL-多々納  09/08/11  退職国保年齢チェック応
      *  04.05.00    NACL-多々納  09/09/XX  '0086'コメントコード対応
      *  04.05.00    NACL-多々納  10/01/XX  治験対応
      *  04.06.00    NACL-多々納  10/08/12  診療科変更対応
      *  04.06.00    NACL-多々納  11/02/16  用法コメント対応
      *  04.08.00    NACL-多々納  14/05/29  一時ファイル変更
      *  04.07.00    NACL-多々納  14/09/XX  Ｈ２６年１０月改正対応
      *  04.08.00    NACL-多々納  16/05/XX  一般名表示対応
      *  04.08.00    NACL-多々納  18/03/XX  Ｈ３０年４月改正対応
      *  04.08.00    NACL-多々納  18/09/XX  選択式コメント対応
      *  05.00.00    NACL-多々納  20/03/XX  Ｒ０１年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME2
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA.
      *
      *    パラメタデータ
      *    SELECT  PARA-FILE2      ASSIGN  FILE-NAME22
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA2.
      *
      *    ＳＰＡデータ
      *    SELECT  SPASCR-FILE     ASSIGN  FILE-NAME1
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-SPASCR.
      *
      *    日毎データ
      *    SELECT  WKSRY-FILE      ASSIGN  FILE-NAME3
      *                            ORGANIZATION    IS  INDEXED
      *                            ACCESS  MODE    IS  DYNAMIC
      *                            RECORD  KEY     IS  WKSRY-F-KEY
      *                            FILE    STATUS  IS  STS-WKSRY.
      *
      *    収納データ
      *    SELECT  OUTSYU-FILE     ASSIGN  FILE-NAME4
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-OUTSYU.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PARA-KEY.
      *        05  PARA-FILEMEI         PIC X(08).
      *        05  PARA-EDANUM          PIC 9(03).
      *    03  PARA-DATA-REC            PIC X(60000).
      *    パラメタデータ
      *FD  PARA-FILE2.
      *    画面スパ領域
      *    COPY    "KT01SCR-SPA"      REPLACING
      *                              //SPA-// BY //PARA-SPA-//.
      *
      *    ＳＰＡパラメタデータ
      *FD  SPASCR-FILE.
      *01  SPA-DATA-REC                 PIC X(1500).
      *
      *    ワーク診療
      *FD  WKSRY-FILE.
      *01  WKSRY-F-REC.
      *    03  WKSRY-F-KEY.
      *        05  WKSRY-F-ZAINUM      PIC 9(08).
      *        05  WKSRY-F-RENNUM      PIC 9(02).
      *    03  WKSRY-DATA-REC          PIC X(2000).
      *
      *    ワーク診療
      *FD  OUTSYU-FILE.
      *01  OUTSYU-REC.
      *    03  OUTSYU-DATA-REC         PIC X(2000).
      *
       WORKING-STORAGE             SECTION.
      *
      * ＳＰＡデータ
      *01  FILE-NAME1.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(10)   VALUE   "KT01SPASCR".
      *    03  FILE-HOSPNUM1       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID1        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(04)   VALUE   ".TXT".
      *    パラメタファイル名
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(08)   VALUE   "KT01PARA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *    パラメタファイル名
      *01  FILE-NAME22.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(09)   VALUE   "KT01PARA2".
      *    03  FILE-HOSPNUM22      PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID22       PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *    パラメタファイル名
      *01  FILE-NAME3.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(09)   VALUE   "KT01WKSRY".
      *    03  FILE-DAY            PIC 9(02)   VALUE   ZERO.
      *    03  FILE-HOSPNUM3       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID3        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *    パラメタファイル名
      *01  FILE-NAME4.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(09)   VALUE   "KT01SYUNO".
      *    03  FILE-DAY4           PIC 9(02)   VALUE   ZERO.
      *    03  FILE-HOSPNUM4       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID4        PIC X(64)   VALUE   SPACE.
      ***  03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "KT01SCR-SPA".
      *
      *    画面共通パラメタ
           COPY    "KT01COMMON-SPA".
      *
      *    フラグ領域
      *01  STS-AREA.
      *    03  STS-PARA            PIC X(02).
      *    03  STS-PARA2           PIC X(02).
      *    03  STS-SPASCR          PIC X(02).
      *    03  STS-WKSRY           PIC X(02).
      *    03  STS-OUTSYU          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-WKSRY           PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-END2            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-KNJBYO          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SPATMP          PIC 9(01).
      *
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-GMN             PIC 9(01).
           03  FLG-KINKI           PIC 9(01).
           03  FLG-KINKI-ARI       PIC 9(01).
           03  FLG-KINKI-ARI2      PIC 9(01).
           03  FLG-ALLFLG          PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-SUJI            PIC 9(01).
           03  FLG-TOROKU          PIC X(01).
           03  FLG-SYORI           PIC X(01).
      *
           03  FLG-SST             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *    初期画面かの判定
           03  FLG-SYOKI           PIC 9(01).
      *
           03  FLG-ARI             PIC 9(01).
      *
           03  FLG-SYOSIN          PIC 9(01).
      *    診察料算定済み
           03  FLG-SINSATU          PIC 9(01).
      *    小児科外来診療科算定
           03  FLG-SYONIKA         PIC 9(01).
      *    算定履歴有無
           03  FLG-JYURR-ARI       PIC 9(01).
      *    時間外加算用
           03  FLG-DELFLG          PIC 9(01).
           03  FLG-ZAIEND          PIC 9(01).
      *----(01.00.01) LINE ADD START ----------------------------------
      *    セット処理有無
           03  FLG-SETSYORI        PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-SET-HEN             PIC 9(01).
           03  FLG-YAKUSOKU        PIC 9(01).
           03  FLG-MAE             PIC 9(01).
      *    剤チェック対象外
           03  FLG-TAISYO          PIC 9(01).
      *    回数入力なし
           03  FLG-KAISU          PIC 9(01).
      *    自費入力
           03  FLG-JIHI           PIC 9(01).
      *
      *    処方せんフラグ
           03  FLG-SYOHOU         PIC 9(01).
           03  FLG-SYOHOU-ARI     PIC 9(01).
      *
           03  FLG-PARASET         PIC 9(01).
      *    検索種別
           03  FLG-KOUI            PIC 9(01).
      *    治癒有無
           03  FLG-KEIZOKU         PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-INS             PIC 9(01).
      *
           03  FLG-KUHAKU          PIC 9(01).
      *
           03  FLG-CHK2            PIC 9(01).
           03  FLG-CHK3            PIC 9(01).
           03  FLG-ARI2            PIC 9(01).
           03  FLG-NYU-ARI         PIC 9(01).
      *    入院中
           03  FLG-NYUIN           PIC 9(01).
      *
           03  FLG-SYUBETU         PIC 9(01).
      *
           03  FLG-KT012TUIKA           PIC 9(01).
      *    入力なし
           03  FLG-INPUT-NASI      PIC 9(01).
      *
           03  FLG-HEN-CHK         PIC 9(01).
      *
           03  FLG-IN1             PIC 9(01).
      *
           03  FLG-FIRUMU           PIC 9(01).
      *
           03  FLG-SONOTA           PIC 9(01).
           03  FLG-NAIFUTAN         PIC 9(01).
      *
           03  FLG-SYOKAI           PIC 9(01).
      *
           03  FLG-RIHTEI           PIC 9(01).
      *    院内・院外処方判定
           03  FLG-INGAIKBN         PIC 9(01).
      *
           03  FLG-TEISEI           PIC 9(01).
      *    薬剤有無
           03  FLG-YAKUZAI-ARI       PIC 9(01).
      *
           03  FLG-SRYKBN            PIC 9(01).
           03  FLG-FUKU-END          PIC 9(01).
           03  FLG-FUKU              PIC 9(01).
           03  FLG-HKN-ARI           PIC 9(01).
           03  FLG-MOD-FLG           PIC 9(01).
      *
      *    画面変更
           03  FLG-HENKOU            PIC 9(01).
      *    初期
           03  FLG-INIT              PIC 9(01).
           03  FLG-MAE-TENSU         PIC 9(01).
      *
           03  FLG-TOUGETU            PIC 9(01).
      *
           03  FLG-WKSRYACT-ARI       PIC 9(01).
      *
           03  FLG-HKNCHK             PIC 9(01).
      *
           03  FLG-KENSADEL            PIC 9(01).
      *
           03  FLG-CHKSET              PIC 9(01).
      *    包括診療用
           03  FLG-HKTCHK              PIC 9(01).
           03  FLG-HOUKATU-ZAI         PIC 9(01).
      *
           03  FLG-NEXT                PIC 9(01).
           03  FLG-DAY-IN              PIC 9(01).
           03  FLG-KAKUNIN             PIC 9(01).
           03  FLG-RRK-ARI             PIC 9(01).
           03  FLG-SANTEI-SYORI        PIC 9(01).
      *
           03  FLG-HKNCOMBICHK         PIC 9(01).
      *
           03  FLG-COPY                PIC 9(01).
           03  FLG-COPYOK              PIC 9(01).
           03  FLG-SRYYM-HEN           PIC 9(01).
      *
           03  FLG-TEI-ARI             PIC 9(01).
           03  FLG-SANTEI              PIC 9(01).
      *    悪性区分前
           03  FLG-MAE-AKUSEI          PIC 9(01).
      *
           03  FLG-INGAI-ARI           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDK1                PIC 9(04).
           03  IDK2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-Z2              PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-SA              PIC 9(04).
      *
           03  IDX-1               PIC 9(04).
      *
           03  IDX-ERR             PIC 9(04).
           03  IDX-SIN             PIC 9(04).
           03  IDX-SIN2            PIC 9(04).
      *
           03  IDX-STR             PIC 9(04).
           03  IDX-STR2            PIC 9(04).
      *
           03  IDX-P               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-KAI             PIC 9(04).
           03  IDX-KAI2            PIC 9(04).
           03  IDX-K1              PIC 9(04).
           03  IDX-K2              PIC 9(04).
           03  IDX-K3              PIC 9(04).
           03  IDX-K4              PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-X               PIC 9(04).
           03  IDX-S2              PIC 9(04).
           03  IDX-IP              PIC 9(04).
           03  IDX-SST             PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-Y3              PIC 9(02).
      *
           03  IDX-KT98             PIC 9(04).
      *
           03  IDX-HKN             PIC 9(04).
           03  IDX-HKN2            PIC 9(04).
           03  IDX-KA              PIC 9(04).
           03  IDX-KA2             PIC 9(04).
           03  IDX-HZN             PIC 9(04).
      *
           03  IDX-GG              PIC 9(04).
      *****03  WRK-MAX             PIC 9(04).
           03  WRK-STR             PIC 9(04).
      *
           03  IDX-SET             PIC 9(04).
           03  IDX-TEI             PIC 9(04).
      *
           03  IDH                 PIC 9(04).
           03  IDH3                PIC 9(04).
      *
           03  IDX-F               PIC 9(04).
           03  IDX-F2              PIC 9(04).
           03  IDX-F3              PIC 9(04).
      *
           03  IDX-NUM             PIC 9(06).
           03  IDX-H1              PIC 9(04).
           03  IDX-H2              PIC 9(04).
      *
           03  IDD2                PIC 9(02).
           03  IDXL                PIC 9(04).
           03  IDD                 PIC 9(02).
           03  IDX-N2              PIC 9(02).
      *
           03  IDX-SN                  PIC 9(04).
           03  IDX-SN2                 PIC 9(04).
      *
           03  RRK-MAX             PIC 9(04).
           03  IDY2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
           03  WRK-SRYYM.
               05  WRK-SRYYY       PIC 9(04).
               05  WRK-SRYMM       PIC 9(02).
           03  WRK-YMD.
               05  WRK-YM          PIC X(04).
               05  WRK-DD          PIC X(02).
           03  WRK-DD-1            PIC 9(02).
      *
           03  WRK-TOUYMD.
               05  WRK-TOUYY       PIC 9(04).
               05  WRK-TOUMM       PIC 9(02).
               05  WRK-TOUDD       PIC 9(02).
      *
           03  WRK-DAY             PIC 9(02).
           03  WRK-BDAY            PIC 9(02).
      *
           03  WRK-FREE            PIC 9(04).
           03  WRK-IDX             PIC 9(03).
      *    入力項目名編集用 
           03  WRK-IDX2            PIC 9(02).
           03  WRK-IDX3            PIC 9(02).
           03  WRK-WIDGET          PIC X(20).
           03  WRK-IN-MAP-IDX      PIC 9(02).
      *    年齢判定用
           03  WRK-BIRTHDAY.
               05  WRK-BIRTH-YY    PIC 9(04).
               05  WRK-BIRTH-MM    PIC 9(02).
               05  WRK-BIRTH-DD    PIC 9(02).
           03  WRK-NYUYOJI         PIC 9(04).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU-Z     PIC ZZZZZZZZ.
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
      *    03  WRK-BUNGSU-X.
     ****      05  WRK-BUNGSU-Z    PIC ZZZZZZZZ.
           03  WRK-GOKEI-X2.
               05  WRK-GOKEI-Z2    PIC ZZZ,ZZZ,ZZZ.
           03  WRK-GOKEI-X3.
               05  WRK-GOKEI-Z3    PIC ---,---,---.
           03  WRK-NOZ             PIC ZZ9.
      *ver.4.7
           03  WRK-TENSU-GX.
               05  WRK-TENSU-G1    PIC X(01).
               05  WRK-TENSU-GZ    PIC ZZZZZZ.
               05  WRK-TENSU-G2    PIC X(01).
      *
      *    剤回数等画面編集用
           03  WRK-KAISU-H         PIC X(04).
      *
           03  WRK-KAISU           PIC 9(04).
           03  WRK-GMN-IDX         PIC 9(04).
      *
           03  WRK-DOSELNUM            PIC 9(04).
           03  WRK-DOSELNUM2           PIC 9(04).
           03  WRK-SELNUM              PIC 9(04).
           03  WRK-SELNUM2             PIC 9(04).
      *
           03  WRK-INPUTCD         PIC X(10).
           03  WRK-SURYO           PIC 9(07)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(07).
               05  WRK-SURYO-S2    PIC 9(05).
      *    03  WRK-BUNGSU          PIC 9(08).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYSYUKBNMEI    PIC X(40).
           03  WRK-SRYKBN          PIC X(02).
      *
           03  WRK-SRYDD           PIC 9(02).
           03  WRK-MAE-SRYDD       PIC 9(02).
      *
      *    退院日
           03  WRK-TAIINYMD        PIC X(08).
           03  WRK-LASTTAIINYMD    PIC X(08).
           03  WRK-GMN-TAIINYMD    PIC X(09).
      *    入院期間
           03  WRK-STDD            PIC 9(02).
           03  WRK-EDDD            PIC 9(02).
           03  IDX-D2              PIC 9(02).
      *
           03  WRK-MOJI            PIC X(01).
      *
           03  WRK-SRYCD           PIC X(09).
           03  WRK-HEN-SRYCD       PIC X(09).
           03  IDX-S               PIC 9(04).
      *    時間外フラグ
           03  WRK-TIMEFLG         PIC X(01).
      *    入力セット処理
           03  WRK-SETCD           PIC X(06).
      *
           03  WRK-KINKIFLG            PIC X(01).
           03  WRK-PTKINKI-ARI         PIC 9(01).
           03  WRK-TUKIJGNFLG          PIC X(01).
           03  WRK-TUKIJGNFLG1         PIC X(01).
      *    算定回数
           03  WRK-SANTEI-CNT          PIC 9(04).
      *    入力位置
           03  WRK-FLG-IN              PIC X(01).
      *    同日初診フラグ
           03  WRK-DOUSIN              PIC 9(01).
      *
      *    診療行為チェック用編集
           03  WRK-TDD                 PIC 9(02).
           03  FLG-ACCTSYORI           PIC 9(01).
      *
           03  FLG-TUIKA               PIC 9(01).
      *
           03  WRK-KIDMSG          PIC X(80).
           03  WRK-KID-SRYCD       PIC X(09).
      *
           03  WRK-MOTOPG          PIC X(08).
      *
      *    労災保険用
           03  WRK-TENSU           PIC 9(08).
           03  WRK-EN              PIC 9(08).
      *
           03  WRK-KEI-ERRCD           PIC X(04).
           03  WRK-KEI-ERRMSG2         PIC X(80).
           03  WRK-ERR-ERRCD           PIC X(04).
           03  WRK-ERR-ERRMSG2         PIC X(80).
           03  WRK-KEI-IDX             PIC 9(04).
      *
      *    老人年齢計算領域
           03  WRK-NENREI.
               05  WRK-NENREI-YY         PIC 9(03).
               05  WRK-NENREI-MM         PIC 9(02).
               05  WRK-NENREI-DD         PIC 9(02).
      *    老人フラグ
           03  WRK-ROUJIN              PIC 9(01).
      *
      *    入力時表示用
           03  IDX-INCNT               PIC 9(04).
           03  FLG-NYURYOKU            PIC 9(01).
           03  WRK-HEN-INPUTCD         PIC X(54).
           03  FLG-IN-ATAI             PIC 9(01).
      *
      *    入力位置判定用
           03  WRK-IN-SURYO            PIC 9(02).
           03  WRK-IN-IDX              PIC 9(04).
           03  WRK-IN-IDX2             PIC 9(04).
           03  WRK-MAX-IDX             PIC 9(04).
      *
           03  WRK-WKSRY-HKNCOMBI      PIC 9(04).
      *
           03  WRK-ERRCD               PIC X(04).
           03  WRK-KIDCD               PIC X(04).
           03  WRK-KIDCD2              PIC X(04).
      *    パス名
           03  WRK-PATH                PIC X(64).
      *
           03  WRK-SAKIPG              PIC X(08).
      *    画面制御
           03  WRK-PUTTYPE             PIC X(08).
      *    日編集
           03  WRK-Z9                  PIC Z9.
           03  WRK-HENDAY              PIC X(02).
      *    保険組合せ保存
           03  WRK-MAE-HKNCOMBI        PIC X(04).
      *
      *    入力名称
           03  WRK-MEISYO-G.
               05  WRK-MEIH                PIC X(02).
      *R02.4
      ****     05  WRK-MEISYO              PIC X(78).
               05  WRK-MEISYO              PIC X(138).
           03  WRK-MEISYO-GHZN.
               05  WRK-MEIHHZN             PIC X(02).
      *R02.4   05  WRK-MEISYOHZN           PIC X(78).
               05  WRK-MEISYOHZN           PIC X(138).
           03  WRK-LEN                 PIC 9(04).
      *    入力値の入力判定用
           03  WRK-HZN-ATAI-AREA.
               05  WRK-HZN-ATAI1           PIC X(10).
               05  WRK-HZN-ATAI2           PIC X(10).
               05  WRK-HZN-ATAI3           PIC X(10).
               05  WRK-HZN-ATAI4           PIC X(10).
               05  WRK-HZN-ATAI5           PIC X(10).
      *
      *    初期業務識別区分
           03  WRK-GYOUMU-KBN          PIC X(04).
      *    ドクターコード
           03  WRK-DRCD.
               05  WRK-DRCD1               PIC X(01).
               05  WRK-DRCD2               PIC X(04).
           03  WRK-DRCD-MEI                PIC X(80).
           03  WRK-SRYKA                   PIC X(02).
           03  WRK-HZN-ERRCD           PIC X(04).
      *
           03  WRK-ERRCD1              PIC X(04).
           03  WRK-ERRCD2              PIC X(04).
           03  WRK-ERRCD3              PIC X(04).
      *
      *    コラムリスト位置
           03  WRK-KT01-ROW             PIC S9(09).
           03  WRK-KT01-ROWATTR         PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
      *    診察料有無
           03  WRK-SINSATU-ARI          PIC 9(01).
      *
      *    小児科外来診察料加算判定
           03  WRK-SYONIKA-KBN         PIC X(01).
      *    訂正時の点数判定
           03  WRK-MAE-TENSU           PIC 9(08).
      *    急性発生日
           03  WRK-KYUSEI-YMD          PIC X(08).
           03  WRK-KYUSEI-END          PIC X(08).
      *
           03  WRK-SYOKAI-YMD          PIC X(08).
      *
           03  WRK-SYORI-KBN           PIC X(01).
      *
           03  WRK-SPA-GMN-SYOSINYMD   PIC X(24).
      *    P97検索名
           03  WRK-P97-NAME            PIC X(22).
           03  WRK-GMN-PTID            PIC 9(10).
      *!
           03  WRK-HOSPNUM             PIC 9(02).
           03  WRK-GRPHOSPUSERID       PIC X(64).
           03  WRK-MACHIPG             PIC X(08).
      *
           03  WRK-LSTDD               PIC 9(02).
      *
      *   年齢表示
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(04).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(08).
      *
       01  WRK-GMNGYO-AREA.
           03  WRK-MAP-MAX         PIC 9(04).
           03  WRK-MAP-CUR         PIC 9(04).
           03  WRK-MAP-CUR2        PIC 9(04).
           03  WRK-MAP-CUR3        PIC 9(04).
           03  WRK-PAGE            PIC 9(04).
           03  WRK-MAP-PAGE        PIC 9(04).
           03  WRK-MAP-PAGE2       PIC 9(04).
           03  WRK-MAP-PAGE3       PIC 9(04).
           03  WRK-MAP-MAXPAGE     PIC 9(04).
      *    画面数量編集用
       01  WRK-GMN-SURYO-AREA.
           03  WRK-SURYO-HENSYU.
               05  WRK-H-SURYO        PIC X(11).
               05  WRK-H-TANINAME     PIC X(04).
               05  WRK-H-F1           PIC X(01).
               05  WRK-H-ZAIKEI       PIC X(14).
               05  WRK-H-KEI          PIC X(08).
      *
           03  WRK-KEI-X.
               05  WRK-KEI-Z       PIC Z(08).
           03  WRK-KEI-X2.
               05  WRK-KEI-Z2      PIC -(08).
           03  WRK-KEI-H           PIC X(08).
           03  WRK-ZAIKEI-H        PIC X(40).
           03  WRK-KAKERU          PIC X(03).
      *
        01  WRK-HENSYU-AREA.
      *    ページ表示
           03  WRK-HEN-PAGE.
               05  WRK-HEN1               PIC X(02).
               05  WRK-HEN-PAGE1          PIC ZZZ.
               05  WRK-HEN2               PIC X(01).
               05  WRK-HEN-PAGE2          PIC ZZ.
      *
           03  WRK-SYOYMDG         PIC X(09).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *画面最大行
       01  MAX-GMN                 PIC 9(04)   VALUE   23.
      *画面ページ数（処理最大行÷画面最大行）
       01  MAX-PAGE                PIC 9(04)   VALUE   17.
       01  MAX-PAGE2               PIC 9(04)   VALUE   18.
      *
       01  WRK-MOJI-AREA.
           03  FLG-SP              PIC 9(02).
           03  FLG-AST             PIC 9(02).
           03  WRK-IDX-FT          PIC 9(02).
           03  WRK-CD-MCNT         PIC 9(02).
           03  WRK-ACCT-MCNT       PIC 9(02).
           03  WRK-SUR-MCNT        PIC 9(02).
           03  WRK-MCNT1           PIC 9(02).
           03  WRK-MCNT2           PIC 9(02).
           03  WRK-MCNT3           PIC 9(02).
           03  WRK-MCNT4           PIC 9(02).
           03  WRK-MCNT5           PIC 9(02).
           03  WRK-CD              PIC X(54).
           03  WRK-KAI             PIC X(20).
           03  WRK-SUR             PIC X(20).
           03  IDM                 PIC 9(04).
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  MAE-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //MAE-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  WRK-HZN-SCR-AREA.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-HZN-//.
      *
       01  WRK-HOZ-IDX             PIC 9(04).
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *
      *    画面スパ領域ワーク
           COPY    "KT01SCR-SPA"      REPLACING
                                     //SPA-// BY //HZN-SPA-//.
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA"   REPLACING 
                                     //SPA-// BY //HZN-SPA-//.
      *
       01  WRK-SYSTIME               PIC 9(08).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    受診履歴（削除）
       01  TBL-JYURRK-AREA.
           02  TBL-JYURRK-REC        OCCURS   31.
           COPY    "CPJYURRK.INC"    REPLACING
                                     //JYURRK-// BY //TBL-JYURRK-//.
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *
      *v4.8
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC". 
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
           COPY    "MCPDATA2.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
      *    COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    負担金額計算用パラメタ
      *D   COPY    "CPORCS01.INC".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    算定履歴チェックパラメタ
      *    COPY    "CPORCSC91.INC".
      *
      *    点数マスタ編集パラメタ
           COPY    "CPORCSC92.INC".
      *
      *    剤分離パラメタ
      *    COPY    "CPORCSC93.INC".
      *
      *    画面再編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC95.INC".
      *
      *    エラーメッセージ編集パラメタ
           COPY    "CPORCSC96.INC".
      *
      *    診療行為入力項目変換パラメタ
           COPY    "CPORCSC97.INC".
      *
      *    投与量等チェックパラメタ
      *    COPY    "CPORCSC98.INC".
      *
      *    包括診療チェックパラメタ
           COPY    "CPORCSCHKTCHK.INC".
      *
      *    診察料自動発生パラメタ
      *    COPY    "CPORCSC10S.INC".
           COPY    "CPORCSCKT10S.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    セット展開サブ
           COPY    "CPORCSCSETHEN.INC".
      *
      *    ワーク診療行為展開・作成
           COPY    "CPORCSCKTWKSRY.INC".
      *    内分泌負荷試験更新パラメタ
      *D   COPY    "CPORCSSANFUKA.INC".
      *
      *    禁忌薬剤チェックパラメタ
           COPY    "CPORCSCKNKCHK.INC".
      *
      *   診療科算定履歴更新サブ
           COPY    "CPORCSKARRK.INC".
      *
      *   算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *    入院履歴編集サブパラメタ
           COPY    "CPORCSCNYUHEN.INC".
      *
      *    診療行為内容編集・計算
           COPY    "CPORCSC100.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    ドクター編集変換サブ
           COPY    "CPORCSDRSET.INC".
      *
      *    診療行為外来更新パラメタ領域
           COPY    "CPORCSCGAIRAI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "KT01.INC".
           COPY    "KT011.INC".
           COPY    "KT012.INC".
           COPY    "KT013.INC".
           COPY    "KT02.INC".
           COPY    "KT03.INC".
           COPY    "KT98.INC".
           COPY    "KTERR.INC".
           COPY    "KTERR2.INC".
           COPY    "KTID1.INC".
           COPY    "KTID2.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
      **** INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  MAE-SCR-REC
           INITIALIZE                  WRK-SCR-REC
           INITIALIZE                  LNK-WKSRYACT-MAX
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-KT01
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    ZERO                TO  FLG-END
           MOVE    ZERO                TO  FLG-END
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-KT01SPA-SET-SEC
      *    画面制御
           MOVE    SPACE           TO  WRK-PUTTYPE
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
      *    画面ＳＰＡ書込み
           IF      FLG-END2            =   ZERO
               PERFORM 1003-KT01SPA-OUT-SEC
           END-IF
      *
           MOVE    "KT01"              TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-KT01            TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KTERR"
                                       OR  "KTERR2"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "KT01"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
          .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別テーブルセット処理
      *****************************************************************
       1001-SRYKBN-SET-SEC             SECTION.
      * 
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           .
       1001-SRYKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡデータＲＥＡＤ処理
      *****************************************************************
       1002-KT01SPA-SET-SEC             SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID1
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
      *
      *    OPEN    INPUT               SPASCR-FILE
      *    IF      STS-SPASCR      NOT =   ZERO
      *        GO      TO      1002-KT01SPA-SET-EXT
      *    END-IF
      *
      *    MOVE    SPACE               TO  SPA-SCR-REC
      *    INITIALIZE          SPA-SCR-REC
      *    MOVE    ZERO                TO  FLG-SPASCR
      *    MOVE    ZERO                TO  IDX
      *    PERFORM UNTIL       FLG-SPASCR  =   1
      *        READ    SPASCR-FILE
      *            AT  END
      *                MOVE   1                TO  FLG-SPASCR
      *            NOT AT  END
      *                ADD    1                TO  IDX
      *                MOVE    SPA-DATA-REC    TO  SPA-GMN-TBL (IDX)
      *        END-READ
      *    END-PERFORM
      *
      *****CLOSE                       SPASCR-FILE
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                      SPA-SCR-REC
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           IF     SPA-HOSPNUM  NOT NUMERIC
               GO   TO  1002-KT01SPA-SET-EXT
           END-IF
      *
      *    TBL_PARA を使用する
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "KT01"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
           MOVE    PARA-REC           TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1 )
                 OR   (IDX                 >=  SPA-MAX-LINE)
               ADD    1                    TO  IDX
               MOVE   PARA-DATA-REC        TO  SPA-GMN-TBL (IDX)
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
      *    行位置とカーソル位置の決定
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL       WRK-IDX     >   SPA-MAX-LINE
               MOVE    ZERO                TO  SPA-COM-PAGE (WRK-IDX)
               MOVE    ZERO                TO  SPA-COM-GYOU (WRK-IDX)
      *        画面表示外のものに頁、行位置を設定する
               IF      SPA-COM-SET    (WRK-IDX)  =   SPACE
                   ADD     1                 TO  IDX
                   IF      IDX               >   MAX-GMN
                       ADD     1             TO  WRK-PAGE
                       MOVE    1             TO  IDX
                   END-IF
      *
                   MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
                   MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
               END-IF
      *
               IF      (SPA-COM-INPUTCD(WRK-IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(WRK-IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
           .
       1002-KT01SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡデータＷＲＩＴＥ処理
      *****************************************************************
       1003-KT01SPA-OUT-SEC             SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID1
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
      *
      *    OPEN    OUTPUT              SPASCR-FILE
      *    IF      STS-SPASCR     NOT =   ZERO
      *        DISPLAY "KT01SPASCR OUT OPEN ERR:"   STS-SPASCR "."
      *    END-IF
      *
      *    PERFORM VARYING     IDX     FROM   1   BY  1
      *            UNTIL      (IDX     >   SPA-MAX-LINE  )
      *       IF       (SPA-GMN-INPUTCD (IDX)     =   SPACE )
      *            AND (SPA-COM-INPUTCD (IDX)     =   SPACE )
      *        AND     (IDX           >   SPA-GMN-MAX  )
      *            CONTINUE
      *        ELSE
      *            MOVE    SPACE               TO  SPA-DATA-REC
      *            MOVE    SPA-GMN-TBL (IDX)   TO  SPA-DATA-REC
      *            WRITE                       SPA-DATA-REC
      *            IF      STS-SPASCR     NOT =   ZERO
      *                DISPLAY "KT01SPASCR OUT WRITE ERR:"
      *                                           STS-SPASCR "."
      *            END-IF
      *       END-IF
      *    END-PERFORM
      *
      **** CLOSE                       SPASCR-FILE
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "KT01"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "KT01"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )
               IF      (SPA-COM-INPUTCD(IDX)   =   SPACE )
                 AND   (SPA-GMN-INPUTCD(IDX)   =   SPACE )
                 AND   (IDX           >   SPA-GMN-MAX  )
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-TBL (IDX)   TO  PARA-DATA-REC
                   MOVE    IDX                 TO  PARA-EDANUM
                   MOVE    PARA-REC            TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "KT01 PARA INSERT ERR:"  MCP-RC
                           ",KEY:" PARA-KEY
                   END-IF
               END-IF
           END-PERFORM
           .
       1003-KT01SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-GMN
           MOVE    SPACE               TO  WRK-KID-SRYCD
      *
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               IF      SPA-MOTOPG          =   "M00"
                   CONTINUE
               ELSE
      *            画面ＳＰＡ読み込み
                   PERFORM 1002-KT01SPA-SET-SEC
      *            元画面へ戻す
                   MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
                   IF      WRK-MOTOPG          =   "P97"
                       MOVE    LNK-COMMON      TO  SPA-KYOUTU
                       MOVE    LNK-SCRSPA      TO  SPA-KT01
                       MOVE    SPA-WORK        TO  SPA-K01KYOUTU
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *        テーブル最大行
               MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *!
               CALL    "ORCSSPACHK"        USING    SPA-AREA
      *!
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "P97"
                   PERFORM 3101-P97SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
           END-IF
      *!
           CALL    "ORCSSPACHK"        USING    SPA-AREA
      *!
      *
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
           MOVE    "2"                 TO  SPA-NYUGAIKBN
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "KT98"
      *@
      *            悪性腫瘍の検査
                   IF     SPA-AKUSEI-FLG       =   1
                       PERFORM 30112-AKUSEI-COMMENT-SEC
                       MOVE    1                   TO  FLG-GMN
                   ELSE
      *TEST
      *H30.9
      *            選択式コメント
                   IF      SPA-SELCOM-FLG      =   1
                       PERFORM 30113-SELCOMMNT-COMMENT-SEC
                   ELSE
      *
                   IF      SPA-KT98-SRYCD-G   NOT =   SPACE
                       PERFORM 3011-KT98-SET-SEC
                       MOVE    1                   TO  FLG-GMN
                   ELSE
                       MOVE    "1"                 TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX)
                       MOVE    2                   TO  FLG-GMN
                   END-IF
      *
                   END-IF
      *
                   END-IF
               WHEN    "KT011"
                   MOVE    ZERO                TO  SPA-NAI-DAYFLG-G
                   MOVE    ZERO                TO  FLG-KAKUNIN
      *            禁忌一覧 ＯＫの時、更新処理へ
                   IF     (SPA-KT011-SYORI      =   2  )  AND
                          (SPA-KT011-CHK        =   1  )
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
                       EVALUATE    SPA-KT011-SAKI
                           WHEN    "KT02"
      *                        更新処理
                               PERFORM 41002-KOUSIN-SYORI-SEC
                       END-EVALUATE
                   END-IF
                   MOVE    2                   TO  FLG-GMN
               WHEN    "KTID1"
      *            確認画面より
                   PERFORM 30010-KTID1-SET-SEC
                   MOVE    2                   TO  FLG-GMN
               WHEN    "KTID2"
      *            確認画面２より
                   PERFORM 30010-KTID2-SET-SEC
                   MOVE    2                   TO  FLG-GMN
               WHEN    "KT012"
      *            ＤＯ選択画面より
                   PERFORM 3013-KT012-SET-SEC
                   MOVE    2                   TO  FLG-GMN
               WHEN    "M00"
      *            業務選択画面より
                   PERFORM 30010-INIT-GMN-SEC
               WHEN    OTHER
      *            外部・患者決定
                   PERFORM 3000-SCREEN-SYORI-SEC
           END-EVALUATE
      *
           IF      FLG-GMN         NOT =   ZERO
               IF      SPA-ERRCD       NOT =   SPACE
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
      *            エラー編集
      *            数量ゼロの場合、エラー表示しない
                   IF      SPA-ERRCD           =   "A001"
                       MOVE    SPACE               TO  SPA-ERRCD
                   ELSE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
               IF     (SPA-KIDCD       NOT =   SPACE ) AND
                      (FLG-END             =   ZERO  )
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
                   PERFORM 520-KIDSET-SEC
               END-IF
           END-IF
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者設定初期画面処理
      *****************************************************************
       3000-SCREEN-SYORI-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-KIDCD2
      *
           MOVE    ZERO                TO  FLG-SYOKAI
           EVALUATE    SPA-MOTOPG
               WHEN    "KT02"
      *            登録後クリア画面の表示
                   IF      SPA-PTNUM          =   SPACE
                       PERFORM 30010-INIT-GMN-SEC
                   ELSE
      *
      *            前回の表示
                   INITIALIZE                  SPA-KT01
                   MOVE    1               TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
      *            特定疾患判定のクリア
                   MOVE    ZERO            TO  SPA-TOKUTEIFLG
                   MOVE    ZERO            TO  SPA-NAIFUKU-CHK
                   MOVE    ZERO            TO  SPA-NAIFUKU-OK
      *H26.10
                   MOVE    ZERO            TO  SPA-PSYCHO-OK
                   MOVE    ZERO            TO  SPA-PSYCHO-CHK
      *H27.4
                   MOVE    ZERO            TO  SPA-TEIGEN30-CHK
                   MOVE    ZERO            TO  SPA-TEIGEN30-OK
      *
                   MOVE    ZERO            TO  SPA-CHOKITOFLG-CHK
                   MOVE    ZERO            TO  SPA-CHOZAI-CHK
                   MOVE    ZERO            TO  SPA-SYOHOU-CHK
                   MOVE    SPACE           TO  SPA-TOKUTEI-SRYKA 
                   MOVE    ZERO            TO  SPA-HKTKEI-FLG
                   MOVE    ZERO            TO  SPA-NAI-SEIKATUCHK
      *!!          手帳加算確認
                   MOVE    ZERO                TO  SPA-TECHOKBN-CHK
      *ver.4.7     緊急時間外確認
                   MOVE    ZERO            TO  SPA-KENJIKAN-CHK
                   MOVE    ZERO            TO  SPA-GAZJIKAN-CHK
      *H28.9       認知症地域包括投薬確認
                   MOVE    ZERO            TO  SPA-NINCHI-CHK
      *!!
      *            最後の日を診療日とする
                   MOVE    SPA-NAI-SRYDD       TO  SPA-GMN-SRYDD
                   MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
      *            診療行為、計計算処理、編集
                   MOVE    SPACE           TO  FLG-TOROKU
                   PERFORM 510-TBLHEN-SEC
                   MOVE    SPACE           TO  FLG-TOROKU
      *
                   END-IF
               WHEN    OTHER
                   INITIALIZE                  SPA-KT01
                   MOVE    1               TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
                   IF      SPA-GMN-PTNUM       =   SPACE
                       PERFORM 30010-INIT-GMN-SEC
                   ELSE
                       PERFORM 3003-GMN-HEN-SEC
                       IF      SPA-MOTOPG      =   "C02"
      *                    病名再編集
                           PERFORM 3004-BYOMEI-HEN-SEC
                       END-IF
                       IF      SPA-MOTOPG      =   "J02"
      *                    会計照会戻り（受診履歴）
                           PERFORM 3004-JYURRK-J02CHK-SEC
                       END-IF
                   END-IF
           END-EVALUATE
      *
           IF      FLG-SYOKAI          =   2
               CONTINUE
           ELSE
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
               MOVE    1               TO  SPA-MAP-IDX
           END-IF
      *
           IF     (SPA-ERRCD       =   SPACE  )  AND
                  (FLG-SYOKAI      =   1      )
      *        システム管理よりカーソル指定
               EVALUATE    SPA-GAI-CURSOR-SET
                   WHEN    "1"
      *                保険組合せ
                       MOVE    05              TO  SPA-GMN-CUR
                   WHEN    "2"
                       MOVE    06              TO  SPA-GMN-CUR
               END-EVALUATE
               IF      LNK-WKSRYACT-MAX        >   ZERO
                   MOVE    01              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       3000-SCREEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｋ０２初期画面表示
      *****************************************************************
       30010-INIT-GMN-SEC              SECTION.
      *
           PERFORM 4900-WKSRYACT-CLEAR-SEC
      *
           INITIALIZE                  SPA-KYOTUKEY
           INITIALIZE                  SPA-KT01
           INITIALIZE                  SPA-SCR-REC
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-KIDCD2
      *
           MOVE    "2"                 TO  SPA-NYUGAIKBN
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SYSYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  LNK-SC95-SRYYMDG
           MOVE    "1"                 TO  LNK-SC95-KBN
           CALL    "ORCSCKT95"         USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-KT01
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    0               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
      *
           MOVE    SPA-SRYYMD      TO  SPA-NAI-SRYYMD
      *
           .
       30010-INIT-GMNN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡのＫ０２を共通ＳＰＡへ
      *****************************************************************
       3003-GMN-HEN-SEC              SECTION.
      *
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    SPA-GMN-KANANAME    TO  SPA-KANANAME
           MOVE    SPA-GMN-SEX         TO  SPA-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAYW
           MOVE    SPA-NAI-BIRTHDAY    TO  SPA-BIRTHDAY
      *
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
           MOVE    SPA-SRYYMD          TO  WRK-SYMD
           PERFORM 520-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-SRYYMDW
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
      *    ドクター
           MOVE    "1"                 TO  SPA-DRCD(1:1)
           MOVE    SPA-GMN-DRCD        TO  SPA-DRCD(2:4)
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
      *
      *    保険組合
           IF      SPA-ERRCD           =   SPACE
               PERFORM 3101-HKNCOMBI-HEN-SEC
           END-IF
      *
           .
       3003-GMN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名再編集処理
      *****************************************************************
       3004-BYOMEI-HEN-SEC            SECTION.
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "B"                 TO  LNK-SC95-KBN
           CALL    "ORCSCKT95"         USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-KT01
      *
           .
       3004-BYOMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計照会変更チェック処理
      *****************************************************************
       3004-JYURRK-J02CHK-SEC            SECTION.
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "N"                 TO  LNK-SC95-KBN
           CALL    "ORCSCKT95"         USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-KT01
      *    受診履歴変更確認
           MOVE    1                   TO  SPA-GMN-CUR
           MOVE    SPA-NAI-LSTDD       TO  WRK-LSTDD
           PERFORM 41013-JYURRK-CHK-SEC
           IF      SPA-NAI-LSTDD   NOT =   WRK-LSTDD
               MOVE    "0178"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-NAI-JYURRKFLG
           END-IF
      *
           .
       3004-JYURRK-J02CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-P97SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM       =   SPA-PTNUM
               CONTINUE
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
      *            初期表示へ
                   MOVE    WRK-SPA-PTNUM   TO  KT01-PTNUM
                   PERFORM 41010-PTNUM-SYORI-SEC
                   MOVE    SPACE           TO  SPA-ERRCD
                   MOVE    ZERO            TO  SPA-NAI-ROUSTR
      *            システム管理よりカーソル指定
                   EVALUATE    SPA-GAI-CURSOR-SET
                       WHEN    "1"
      *                保険組合せ
                           MOVE    05              TO  SPA-GMN-CUR
                       WHEN    "2"
      *                診療科
      *                受付一覧からの時
                           IF      WRK-MOTOPG          =   "U01"
                               MOVE    01              TO  SPA-GMN-CUR
                           ELSE
                               MOVE    06              TO  SPA-GMN-CUR
                           END-IF
                       WHEN    OTHER
                           MOVE    01              TO  SPA-GMN-CUR
                   END-EVALUATE
                   IF      LNK-WKSRYACT-MAX        >   ZERO
                       MOVE    01              TO  SPA-GMN-CUR
                   END-IF
      *            入院中の時、カーソルを患者番号へ
                   IF      SPA-KIDCD           =   "0121"
                        MOVE    ZERO           TO  SPA-GMN-CUR
                    END-IF
               END-IF
           END-IF
      *
           IF     (SPA-PTID            =   ZERO  )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               MOVE    ZERO                TO  SPA-GMN-CUR
      **** ELSE
      *****    MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       3101-P97SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧画面（Ｋ９８）ＯＫ処理
      *****************************************************************
       3011-KT98-SET-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-IN-MAP-IDX
           MOVE    SPA-GMN-IDX         TO  WRK-STR
      *
           PERFORM VARYING     IDX-KT98 FROM    1   BY  1
                   UNTIL      (IDX-KT98             >   10  )  OR
                              (SPA-KT98-SRYCD(IDX-KT98)  =   SPACE )  OR
                              (SPA-ERRCD           NOT =   SPACE )
      *
               IF      IDX-KT98             =   1
                   INITIALIZE                  SPA-COM-TBLREC
                                                       (SPA-GMN-IDX)
                   INITIALIZE                  SPA-GMN-TBLREC
                                                       (SPA-GMN-IDX)
                   MOVE    SPA-KT98-SRYCD(IDX-KT98)
                                           TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
               ELSE
                   ADD     1               TO  SPA-GMN-IDX
                   PERFORM 41014-GYOINSN-SEC
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    SPA-KT98-SRYCD(IDX-KT98)
                                               TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "1"             TO  SPA-COM-IN (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN2(SPA-GMN-IDX)
               END-IF
      *
           END-PERFORM
      *
      *    入力コード・名称チェック処理
           PERFORM 410112-KOUI-SPACHK-SEC
      *
           .
       3011-KT98-SET-EXT.
           EXIT.
      *@@
      *****************************************************************
      *    悪性腫瘍検査一覧へ処理
      *****************************************************************
       5101-AKUSEI-KT98-SEC       SECTION.
      *
           MOVE    SPA-AKUSEI-IDX      TO  SPA-GMN-IDX
      *
           INITIALIZE                      SPA-KT98-AREA
           MOVE    "//*D009S"          TO  SPA-KT98-INPUTCD
           MOVE    "A"                 TO  SPA-KT98-CDSYUBETU
           MOVE    "3"                 TO  SPA-COM-CUR (SPA-AKUSEI-IDX)
      *
      *    診療行為一覧へ
           PERFORM 410-KT98-SYORI-SEC
           .
       5101-AKUSEI-KT98-EXT.
           EXIT.
      *****************************************************************
      *    悪性腫瘍検査一覧へ処理
      *****************************************************************
       30112-AKUSEI-COMMENT-SEC       SECTION.
      *
           ADD     1                   TO  SPA-GMN-IDX
           IF      SPA-GMN-IDX         <=  SPA-GMN-MAX
               PERFORM 41014-GYOINSN-SEC
           END-IF
           MOVE    SPA-GMN-IDX         TO  IDX-S
           IF      SPA-KT98-SRYCD-G   NOT =   SPACE
               PERFORM 3011-KT98-SET-SEC
           ELSE
      *    コメントコード追加
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "830000015"     TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN2(SPA-GMN-IDX)
      *           入力コード・名称チェック処理
                  PERFORM 410112-KOUI-SPACHK-SEC
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-AKUSEI-FLG
           MOVE    ZERO                TO  SPA-AKUSEI-IDX
      *
           .
       30112-AKUSEI-COMMENT-EXT.
           EXIT.
      *
      *H30.9
      *****************************************************************
      *    選択式コメントへ処理
      *****************************************************************
       30113-SELCOMMNT-COMMENT-SEC       SECTION.
      *
           IF     (SPA-SELCOM-IDX      >   ZERO  )
             AND  (SPA-COM-INPUTCD(SPA-SELCOM-IDX)(1:1)  =   "1"  )
             AND  (SPA-COM-RECEKISAI (SPA-SELCOM-IDX)    =   "1"
                                                         OR  "2"  )
      *        選択済み
               MOVE    "1"             TO  SPA-COM-RECEAUTO
                                                    (SPA-SELCOM-IDX)
           END-IF
      *
      *    対象行の下に挿入
           IF      SPA-COM-INPUTCD (SPA-GMN-IDX) NOT =  SPACE
               ADD     1                   TO  SPA-GMN-IDX
               IF      SPA-GMN-IDX         <=  SPA-GMN-MAX
                   PERFORM 41014-GYOINSN-SEC
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-SELCOM-FLG
           MOVE    ZERO                TO  SPA-SELCOM-IDX
      *
           PERFORM 3011-KT98-SET-SEC
           MOVE    1                   TO  FLG-GMN
      *
           .
       30113-SELCOMMNT-COMMENT-EXT.
           EXIT.
      *
      *@@
      *
      *****************************************************************
      *    確認画面（ＫＩＤ１）ＯＫ処理
      *****************************************************************
       30010-KTID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KIDCD
      *        一括削除
               WHEN    "3004"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 21011-ALL-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *
               WHEN    "0102"
                   MOVE    SPACE               TO  SPA-KIDCD
      *            外来加算料削除
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPA-CHK-IDX     TO  SPA-GMN-IDX
      *                診療料をクリアする
                       MOVE    SPACE           TO  SPA-GMN-INPUTCD
                                                        (SPA-GMN-IDX)
      *
      *                診療行為、計計算処理、編集
                       MOVE    "1"             TO  FLG-TOROKU
                       PERFORM 510-TBLHEN-SEC
                       MOVE    SPACE           TO  FLG-TOROKU
      *
                       PERFORM 500-GMNHEN-SEC
      *
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
                       PERFORM 4100-TOROKU-SEC
      *
                   END-IF
      *
               WHEN    "0104"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                終了処理へ
                       PERFORM 210-BACK-OK-SEC
                   END-IF
      *        保険組合せ変更確認
               WHEN    "0105"
                   MOVE    SPACE               TO  SPA-KIDCD
                   PERFORM 41014-HKN-CHANGE-SYORI-SEC
      *
      *        初回算定日の確認
               WHEN    "0140"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPACE               TO  SPA-KIDCD
                       PERFORM 3001099-SYOKAISET-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *
      *        特定疾患処方管理加算の算定確認
               WHEN    "0113"
               WHEN    "0213"
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    2                   TO  SPA-TOKUTEIFLG
                   ELSE
                       MOVE    1                   TO  SPA-TOKUTEIFLG
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
                   MOVE    1                   TO  FLG-KAKUNIN
      *            画面制御
                   MOVE    "JOIN"              TO  WRK-PUTTYPE
                   PERFORM 41002-KOUSIN-SYORI-SEC
      *        長期投与加算の算定確認
               WHEN    "0130"
               WHEN    "0230"
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    2                   TO  SPA-CHOKITOFLG
                   ELSE
                       MOVE    1                   TO  SPA-CHOKITOFLG
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
                   MOVE    1                   TO  FLG-KAKUNIN
      *            画面制御
                   MOVE    "JOIN"              TO  WRK-PUTTYPE
                   PERFORM 41002-KOUSIN-SYORI-SEC
      *!!
      *平成２２年４月から
      *        手帳加算の算定確認
               WHEN    "2003"
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    2                   TO  SPA-TECHOKBN-CHK
                   ELSE
                       MOVE    1                   TO  SPA-TECHOKBN-CHK
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *            画面制御
                   MOVE    "JOIN"              TO  WRK-PUTTYPE
                   PERFORM 41002-KOUSIN-SYORI-SEC
      *!!
      *
      *        クリアの確認
               WHEN    "0116"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 220-CLEAR-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *
      *        診療科変更確認
               WHEN    "0123"
                   MOVE    SPACE               TO  SPA-KIDCD
                   PERFORM 41014-CHANGE-SYORI-SEC
      *
      *        同一検査削除へ
               WHEN    "0160"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 30016-KENSA-DELETE-SEC
                   END-IF
      *
      *        包括分入力変更確認
               WHEN    "0190"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                      INITIALIZE                  SPA-SCR-REC
                   END-IF
      *
      *        内服７種類以上逓減確認
               WHEN    "2001"
               WHEN    "2002"
      *ver.4.7
               WHEN    "2004"
               WHEN    "2005"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                      MOVE    1                TO  SPA-NAIFUKU-OK
                   ELSE
                      MOVE    ZERO             TO  SPA-NAIFUKU-OK
                   END-IF
                   MOVE    1                   TO  FLG-KAKUNIN
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *            画面制御
                   MOVE    "JOIN"              TO  WRK-PUTTYPE
                   PERFORM 41002-KOUSIN-SYORI-SEC
      *H26.10
      *        向精神薬剤多剤投与
               WHEN    "2006"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                      MOVE    1                TO  SPA-PSYCHO-OK
                   ELSE
                      MOVE    ZERO             TO  SPA-PSYCHO-OK
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *            画面制御
                   MOVE    "JOIN"              TO  WRK-PUTTYPE
                   PERFORM 41002-KOUSIN-SYORI-SEC
      *H27.4
      *        低紹介率３０日以上投与
               WHEN    "2007"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                      MOVE    1                TO  SPA-TEIGEN30-OK
                   ELSE
                      MOVE    ZERO             TO  SPA-TEIGEN30-OK
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *            画面制御
                   MOVE    "JOIN"              TO  WRK-PUTTYPE
                   PERFORM 41002-KOUSIN-SYORI-SEC
      *
      *        クリア確認チェック
               WHEN    "0125"
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 2221-PTNUMCLEAR-SYORI-SEC
                   ELSE
                        MOVE    1               TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *@
      *        包括エラーの削除
               WHEN    "0150"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPACE               TO  SPA-KIDCD
                       PERFORM 51091-HKTSANTEI-DEL-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *        会計照会変更後
               WHEN    "0152"
                   MOVE    SPACE               TO  SPA-KIDCD
                   PERFORM 230-MAEKPTNUM-SEC
      *!!
               WHEN    "0138"
                   MOVE    SPACE               TO  SPA-KIDCD
                   PERFORM 41013-SRYKA-CHANGE-HKN02-SEC
      *!!
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KID1-FLG
      *****MOVE    SPACE               TO  SPA-KIDCD
           .
       30010-KTID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＫＩＤ２）ＯＫ処理
      *****************************************************************
       30010-KTID2-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KTID2CD
      *        診察料の自動発生
               WHEN    "3001"
                   MOVE    SPACE               TO  SPA-KTID2CD
                   EVALUATE    SPA-KTID2-FLG
                       WHEN    "1"
      *                初診
                           MOVE    1               TO  FLG-SINSATU
                           PERFORM 300109-SINSTU-HEN-SEC
                       WHEN    "2"
      *                再診
                           MOVE    2               TO  FLG-SINSATU
                           PERFORM 300109-SINSTU-HEN-SEC
                   END-EVALUATE
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KTID2-FLG
           MOVE    SPACE               TO  SPA-KTID2CD
           .
       30010-KTID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    初診料の履歴算定、再診料の再変更処理
      *****************************************************************
       3001099-SYOKAISET-SEC     SECTION.
      *
      *    算定履歴作成
           INITIALIZE                  ORCSSANTEIAREA
           MOVE    "1"                 TO  SSANTEI-SYORI
           MOVE    "K03"               TO  SSANTEI-PG
           MOVE    SPA-SYOKAI-SRYCD    TO  SSANTEI-SRYCD
           MOVE    SPA-SYOKAI-YMD      TO  SSANTEI-SRYYMD
           MOVE    SPA-PTID            TO  SSANTEI-PTID
           MOVE    1                   TO  SSANTEI-KAISU
           MOVE    SPA-SRYKA           TO  SSANTEI-SRYKA
           MOVE    SPA-HKNCOMBINUM     TO  SSANTEI-HKNCOMBINUM
           MOVE    SPA-HKNKBN          TO  SSANTEI-HKNKBN
           MOVE    SPA-RSI-HKNKBN      TO  SSANTEI-RSI-HKNKBN
           MOVE    SPA-RSI-JUNKYO      TO  SSANTEI-RSI-JUNKYO
           CALL    "ORCSSANTEI"        USING
                                       ORCSSANTEIAREA
                                       SPA-AREA
           IF      SPA-ERRCD           =   "9000"
      *        DB更新エラー
               MOVE    1                   TO  FLG-END2
           END-IF
      *
      *    診療科履歴更新
           INITIALIZE                  ORCSKARRKAREA
           MOVE    SPA-SRYYMD(1:6)     TO  SKARRK-SRYYM
           MOVE    SPA-SRYKA           TO  SKARRK-SRYKA
           CALL    "ORCSKARRK"         USING
                                       ORCSKARRKAREA
                                       SPA-AREA
           IF      SKARRK-RC       NOT =   ZERO
               MOVE    1                   TO  FLG-END2
           END-IF
           MOVE    SPA-SYOKAI-YMD      TO  SPA-SYOYMD
           MOVE    SPACE               TO  SPA-SYOSIN-YMD
           MOVE    SPACE               TO  SPA-SYOSIN-SRYCD
           MOVE    ZERO                TO  SPA-SYOSIN
      *    初診算定日
           MOVE    SPA-SYOYMD          TO  WRK-SYMD
           PERFORM 520-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-SYOSINYMD
      *    画面用スパ編集処理
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    ZERO                TO  FLG-TEISEI
           PERFORM 310-SPA-GMNSET-SEC
           MOVE    SPACE               TO  SPA-KID1-FLG
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-MAP-IDX
      *
           MOVE    ZERO                TO  SPA-SYOKAI-IDX
           MOVE    SPACE               TO  SPA-SYOKAI-YMD
           MOVE    SPACE               TO  SPA-SYOKAI-SRYCD
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       3001099-SYOKAISET-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一検査削除処理
      *****************************************************************
       30016-KENSA-DELETE-SEC                  SECTION.
      *
      *    診療行為、計計算処理、編集
           MOVE    1                   TO  FLG-KENSADEL
           MOVE    SPACE               TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    ZERO                TO  FLG-KENSADEL
           MOVE    SPACE               TO  FLG-TOROKU
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       30016-KENSA-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料自動発生処理
      *****************************************************************
       300109-SINSTU-HEN-SEC                  SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSCKT10SAREA
           IF      FLG-SINSATU         =   1
               MOVE    "1"                 TO  ORCSCKT10S-SYORI
           ELSE
               MOVE    "2"                 TO  ORCSCKT10S-SYORI
           END-IF
           CALL    "ORCSCKT10S"        USING
                                       ORCSCKT10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-KT01
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
           .
       300109-SINSTU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ選択（ＫＴ０１２）画面より処理
      *****************************************************************
       3013-KT012-SET-SEC                  SECTION.
      *
      *    選択がない時、そのまま
           IF      SPA-KT012-SELOK       =   ZERO
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      3013-KT012-SET-EXT
           END-IF
      *
      *    パラメタファイルより表示
           MOVE    ZERO            TO  FLG-PARASET
           PERFORM 3002-PARA-SET-SEC
      *
      *    ＤＯ画面（Ｋ０９）より、ＤＯ内容追加編集
           MOVE    SPA-GMN-MAX         TO  IDX2
           MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
           MOVE    SPA-GMN-MAX         TO  WRK-STR
      *
      *    再表示編集処理
           IF     (SPA-KT012-SELOK     =   1    )  AND
                  (LNK-WKSRYACT-MAX    >   ZERO )
               MOVE    ZERO            TO  FLG-SYOSIN
               MOVE    1               TO  FLG-KT012TUIKA
               MOVE    SPACE           TO  WRK-ERRCD1
               PERFORM 3103-SAIHEN-HEN-SEC
      *
               IF      WRK-ERRCD1      NOT =   SPACE
                   MOVE    WRK-ERRCD1          TO  SPA-ERRCD
               END-IF
      *
      *        診療行為、計計算処理、編集
               MOVE    ZERO            TO  FLG-INIT
               MOVE    SPACE           TO  FLG-TOROKU
               MOVE    1               TO  FLG-SYOKI
               PERFORM 510-TBLHEN-SEC
               MOVE    SPACE           TO  FLG-TOROKU
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       3013-KT012-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     パラメタファイルより編集処理
      *****************************************************************
       3002-PARA-SET-SEC                  SECTION.
      *
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA.
      *
      ***  MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    INPUT               PARA-FILE
      *
      *    IF      STS-PARA           =  ZERO
      *        PERFORM 980-PARA-READ-SEC
      *    ELSE
      *        MOVE    2               TO  FLG-PARA 
      *    END-IF
      *    PERFORM
      *        UNTIL   FLG-PARA        NOT =   ZERO
      *        IF      PARA-FILEMEI        =   "WKSRYACT"
      *            IF      PARA-DATA-REC   NOT =   SPACE
      *                ADD     1                   TO  LNK-WKSRYACT-MAX
      *                IF      LNK-WKSRYACT-MAX    >   SPA-MAX-LINE
      *                    DISPLAY "KT01 LNK-WKSRYACT-MAX 400 OVR"
      *                    MOVE    "9000"          TO  SPA-ERRCD
      *                ELSE
      *                    MOVE    PARA-DATA-REC   TO  LNK-WKSRYACT-REC
      *                                               (LNK-WKSRYACT-MAX)
      *                END-IF
      *            END-IF
      *        END-IF
      *
      *        IF      PARA-FILEMEI        =   "KT01PARA"
      *            IF      FLG-PARASET     =   1
      *                MOVE    PARA-DATA-REC       TO  SPA-K01KYOUTU
      *            END-IF
      *        END-IF
      *        PERFORM 980-PARA-READ-SEC
      *    END-PERFORM
      *
      **** CLOSE                       PARA-FILE
      *
      *ver.4.8
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "KT01"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1     )
                  OR  (SPA-ERRCD       NOT =   SPACE )
               ADD     1                   TO  LNK-WKSRYACT-MAX
               IF      LNK-WKSRYACT-MAX    >   SPA-MAX-LINE
                   DISPLAY "KT01 LNK-WKSRYACT-MAX 400 OVR"
                   MOVE    "9000"              TO  SPA-ERRCD
               ELSE
                   MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
                                                      (LNK-WKSRYACT-MAX)
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           IF      FLG-PARASET         =   1
               INITIALIZE                      SPATMP-REC
               MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
               MOVE    "KT01"              TO  SPATMP-GYOUMUID
               MOVE    SPA-TERMID          TO  SPATMP-TERMID
               MOVE    "KT01PARA"          TO  SPATMP-FILEMEI
               MOVE    1                   TO  SPATMP-EDANUM
               PERFORM 9100-SPATMP-KEYREAD-SEC
               IF      FLG-SPATMP          =   ZERO
                   MOVE    SPATMP-DATA-REC     TO  SPA-K01KYOUTU
               END-IF
           END-IF
      *
           IF      FLG-PARASET         =   1
      ******   MOVE    SPA-TERMID          TO  FILE-TERMID22
      *        MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM22
      *        OPEN    INPUT               PARA-FILE2
      *
      *        IF      STS-PARA2           =  ZERO
      *            READ    PARA-FILE2
      *                AT  END
      *                    INITIALIZE                  PARA-SPA-KT01
      *                NOT AT  END
      *                    MOVE    PARA-SPA-KT01       TO  SPA-KT01
      *            END-READ
      *        END-IF
      *******  CLOSE                       PARA-FILE2
      *
               INITIALIZE                      SPATMP-REC
               MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
               MOVE    "KT01"              TO  SPATMP-GYOUMUID
               MOVE    SPA-TERMID          TO  SPATMP-TERMID
               MOVE    "SPAKT01"           TO  SPATMP-FILEMEI
               MOVE    1                   TO  SPATMP-EDANUM
               PERFORM 9100-SPATMP-KEYREAD-SEC
               IF      FLG-SPATMP          =   ZERO
                   MOVE    SPATMP-DATA-REC     TO  SPA-KT01
               END-IF
           END-IF
      *
           .
       3002-PARA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
      *    見出し
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-NAME            TO  SPA-GMN-NAME
           MOVE    SPA-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    SPA-SEX             TO  SPA-GMN-SEX
           MOVE    SPA-BIRTHDAYW       TO  SPA-GMN-BIRTHDAY
           MOVE    SPA-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM(1:6)
           MOVE    SPA-SRYYMD (7:2)    TO  SPA-GMN-SRYDD 
           MOVE    SPA-BIRTHDAY        TO  SPA-NAI-BIRTHDAY
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
           MOVE    SPA-SRYKA           TO  SPA-GMN-SRYKA
           MOVE    SPA-FTNRATE         TO  SPA-GMN-FTNRATE
      *
      *    診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKALST (IDX)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
      *    保険組合
           MOVE    SPA-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBINUM
           PERFORM 3101-HKNCOMBI-HEN-SEC
      *
      *    最終来院日
           MOVE    SPACE               TO  SPA-GMN-LASTYMD-G
           IF      SPA-LSTYMD      NOT =   SPACE
               MOVE    SPA-LSTYMD      TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-LASTYMD
           END-IF
      *
      *    初診算定日
           IF     (SPA-SYOYMD      NOT =   SPACE )
               MOVE    SPA-SYOYMD      TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-SYOSINYMD
      *        同時初診
               IF     (SPA-DOUYMD      NOT =   SPACE )
                 AND  (SPA-DOUYMD          >=  SPA-SYOYMD)
                   MOVE    WRK-HENYMDG     TO  WRK-SYOYMDG
                   MOVE    SPACE           TO  SPA-GMN-SYOSINYMD
                   MOVE    SPA-DOUYMD      TO  WRK-SYMD
                   PERFORM 520-SEIWA-HEN-SEC
                   STRING  WRK-SYOYMDG
                           " ("
                           WRK-HENYMDG
                           ")"             DELIMITED  BY   SIZE
                                           INTO    SPA-GMN-SYOSINYMD
                   END-STRING
               END-IF
           END-IF
      *
      *    年齢計算
           PERFORM 3102-AGECHK-HEN-SEC
      *
      *    診療日の末日を求める
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-SRYYMD(1:6)     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-SRYLST-DD
           ELSE
               MOVE    31                  TO  SPA-SRYLST-DD
           END-IF
      *
      *    入院判定
           MOVE    SPACE               TO  SPA-KIDCD
           PERFORM 3109-NYUIN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      310-SPA-SET-EXT
           END-IF
      *
           MOVE    ZERO                TO  SPA-NAI-SYORI
      *    最終退院日
           IF      SPA-NAI-LASTTAIIN   =   SPACE
               MOVE    SPACE               TO  WRK-GMN-TAIINYMD
           ELSE
               MOVE    SPA-NAI-LASTTAIIN   TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  WRK-GMN-TAIINYMD
               MOVE    SPA-GMN-LASTYMD     TO  WRK-HENYMDG
      *        最終来院日
               MOVE    SPACE               TO  SPA-GMN-LASTYMD-G
               STRING  WRK-HENYMDG         DELIMITED  BY  SIZE
                       " ("                DELIMITED  BY  SIZE
                       WRK-GMN-TAIINYMD    DELIMITED  BY  SIZE
                       ")"                 DELIMITED  BY  SIZE
                                           INTO    SPA-GMN-LASTYMD-G
               END-STRING
           END-IF
           IF     (WRK-GMN-TAIINYMD    =   SPACE )  AND
                  (SPA-BEDFLG          =   ZERO  )
      *        入院なし
               MOVE    "最終来院日"            TO  SPA-GMN-LASTMID
           ELSE
               MOVE    "最終来院日（退院日）"  TO  SPA-GMN-LASTMID
           END-IF
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢計算編集処理
      *****************************************************************
       3102-AGECHK-HEN-SEC              SECTION.
      *
      *    年齢計算
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-NENREI2-YY   TO  SPA-NENREI-YY
           MOVE    AGECHK-NENREI2-MM   TO  SPA-NENREI-MM
           MOVE    AGECHK-NENREI2-DD   TO  SPA-NENREI-DD
      *
      *    画面表示用
           IF      SPA-NENREI-YY       =   ZERO
               MOVE    SPA-NENREI-MM       TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    SPA-NENREI-YY       TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-NENREI-HEN
      *H28.4
      *    未就学前
           MOVE    AGECHK-SYUGAKUKBN   TO  SPA-SYUGAKUKBN
      *
           .
       3102-AGECHK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合決定編集処理
      *****************************************************************
       3101-HKNCOMBI-HEN-SEC              SECTION.
      *
           MOVE    SPA-GMN-HKNCOMBINUM     TO  SPA-HKNCOMBINUM
      *
      *    保険組合せ詳細編集
           INITIALIZE                  ORCSC95AREA
           MOVE    "H"                 TO  LNK-SC95-KBN
           CALL    "ORCSCKT95"         USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-KT01
           .
       3101-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面用スパ編集処理
      *****************************************************************
       310-SPA-GMNSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      310-SPA-GMNSET-EXT
           END-IF
      *
      *    再表示編集処理
           IF      LNK-WKSRYACT-MAX    >   ZERO
               MOVE    ZERO            TO  FLG-KT012TUIKA
               PERFORM 3103-SAIHEN-HEN-SEC
           END-IF
      *
      *    入力コード・名称チェック処理
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
      *
               IF      SPA-COM-CUR (IDX)   =   SPACE
                   MOVE    SPA-GMN-INPUTCD(IDX)
                                           TO  SPA-MAE-INPUTCD(IDX)
               END-IF
      *        警告表示をしない
               MOVE    "1"                 TO  SPA-COM-KZMFLG(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG3(IDX)
               MOVE    "1"                 TO  SPA-COM-HKEIFLG(IDX)
           END-PERFORM
      *    内分泌負荷試験検査情報
           INITIALIZE                  SPA-NAI-FUKATEN-AREA
      *    診療行為、計計算処理、編集
           MOVE    1                   TO  FLG-INIT
           MOVE    SPACE               TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
      *    訂正時点数変更チェック
           MOVE    ZERO                TO  SPA-TEISEI-CHK
           MOVE    ZERO                TO  SPA-TEISEI-ERR
      *    警告フラグクリア
           MOVE    ZERO                TO  WRK-MAE-TENSU
           MOVE    ZERO                TO  FLG-MAE-TENSU
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
               IF     (SPA-COM-KZMFLG (IDX)    NOT =   SPACE )  OR
                      (SPA-COM-KZMFLG2(IDX)    NOT =   SPACE )  OR
                      (SPA-COM-KZMFLG3(IDX)    NOT =   SPACE )
      *            約束セット
                   IF     (SPA-GMN-INPUTCD(IDX)(1:1)   =   "." )
                      OR  (SPA-GMN-INPUTCD(IDX)(1:1)   =   "#" )
                      OR  (SPA-GMN-INPUTCD(IDX)(1:1)   =   "$" )
      *?              OR  (SPA-GMN-INPUTCD(IDX)(1:1)   =   "S" )
                      CONTINUE
                   ELSE
                       MOVE    SPACE       TO  SPA-MAE-INPUTCD (IDX)
                   END-IF
               END-IF
      *        警告フラグクリア
               MOVE    SPACE           TO  SPA-COM-KZMFLG (IDX)
               MOVE    SPACE           TO  SPA-COM-KZMFLG2(IDX)
               MOVE    SPACE           TO  SPA-COM-KZMFLG3(IDX)
               MOVE    SPACE           TO  SPA-COM-KZMFLG4(IDX)
               MOVE    SPACE           TO  SPA-COM-KZMFLG5(IDX)
               MOVE    SPACE           TO  SPA-COM-KZMFLG6(IDX)
               MOVE    SPACE           TO  SPA-COM-KZMFLG7(IDX)
               MOVE    SPACE           TO  SPA-COM-KINKIFLG(IDX)
               MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG (IDX)
               MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG1(IDX)
      *        併用算定が警告の時、クリア
               IF      SPA-COM-HKEIFLG (IDX)   =   "1"
                   MOVE    SPACE               TO  SPA-COM-HKEIFLG (IDX)
                   MOVE    SPACE               TO  SPA-COM-HCHKFLG (IDX)
               END-IF
      *        訂正時の点数判定
               IF      SPA-SYORIFLG        =   1
                   IF      FLG-MAE-TENSU   =   ZERO
                       IF     (SPA-COM-MAE-TENSU(IDX)  >   ZERO  )  OR
                              (SPA-COM-INPUTCD (IDX)   NOT =   SPACE)
                           IF     (SPA-COM-MAE-TENSU(IDX)  =   ZERO) AND
                                  (SPA-COM-INPUTCD (IDX)(1:1)  =  "8"
                                                               OR "0")
                               CONTINUE
                           ELSE
                               MOVE    SPA-COM-MAE-TENSU(IDX)
                                                   TO  WRK-MAE-TENSU
                               MOVE    1           TO  FLG-MAE-TENSU
                           END-IF
                       END-IF
                   END-IF
                   IF     (SPA-COM-ZAIEND(IDX)     =   "1" )  OR
                         ((SPA-COM-SRYKBN(IDX)     =   "95"
                                                   OR  "96" ) AND
                          (SPA-COM-JIHIMONEY(IDX)  >   ZERO ))
                       IF      SPA-COM-SRYKBN(IDX)     =   "95"
                                                       OR  "96"
                           MOVE    SPA-COM-JIHIMONEY(IDX)
                                                   TO  WRK-TENSU
                       ELSE
                           MOVE    SPA-COM-TENSU (IDX)
                                                   TO  WRK-TENSU
                       END-IF
                       IF      WRK-TENSU       NOT =   WRK-MAE-TENSU
                           MOVE    1               TO  SPA-TEISEI-ERR
                           MOVE    "1"             TO  SPA-COM-CUR (IDX)
                       END-IF
                       MOVE    ZERO            TO  WRK-MAE-TENSU
                       MOVE    ZERO            TO  FLG-MAE-TENSU
                   END-IF
                   IF      SPA-COM-ZAIEND(IDX)     =   "1"
                       MOVE    ZERO                TO  WRK-MAE-TENSU
                       MOVE    ZERO                TO  FLG-MAE-TENSU
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-KIDCD
      *    内分泌負荷試験検査情報
           INITIALIZE              SPA-NAI-FUKATEN-AREA
      *
           IF      WRK-ERRCD1      NOT =   SPACE
               MOVE    WRK-ERRCD1          TO  SPA-ERRCD
           END-IF
      *
           IF     (SPA-TEISEI-ERR      =   1  )  AND
                  (SPA-ERRCD           =   SPACE)
               MOVE    "K999"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-TEISEI-CHK
           END-IF
      *
           .
       310-SPA-GMNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       3103-SAIHEN-HEN-SEC             SECTION.
      *
           INITIALIZE                  ORCSCKTWKSRYAREA
           MOVE    "4"                 TO  ORCSCWKSRY-KBN
           MOVE    "KT01"              TO  ORCSCWKSRY-PG
           MOVE    IDX2                TO  ORCSCWKSRY-STRIDX
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCKTWKSRY"      USING
                                       ORCSCKTWKSRYAREA
                                       SPA-AREA
                                       SPA-KT01
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           IF      ORCSCWKSRY-ERRCD    NOT =   SPACE
               MOVE    ORCSCWKSRY-ERRCD    TO  WRK-ERRCD1
           END-IF
           IF      ORCSCWKSRY-ERRCD2   NOT =   SPACE
               MOVE    ORCSCWKSRY-ERRCD2   TO  WRK-ERRCD3
           END-IF
      *
      *    INITIALIZE                  ORCSCWKSRYAREA
      *    MOVE    "1"                 TO  ORCSCWKSRY-KBN
      *    MOVE    "KT01"               TO  ORCSCWKSRY-PG
      *    処理区分（０：訂正展開、１：中途終了展開、２：ＤＯ展開）
      *    IF      FLG-KT012TUIKA        =   1
      *        MOVE    2               TO  ORCSCWKSRY-SYORIFLG
      *    ELSE
      *        MOVE    1               TO  ORCSCWKSRY-SYORIFLG
      *        IF      SPA-SYORIFLG    =   1
      *            MOVE    ZERO            TO  ORCSCWKSRY-SYORIFLG
      *        END-IF
      *    END-IF
      *    中途終了分追加フラグ
      *    IF      FLG-KT012TUIKA        =   3
      *       MOVE    1                TO  ORCSCWKSRY-KT012TUIKA
      *    END-IF
      *    追加開始位置
      *    MOVE    IDX2                TO  ORCSCWKSRY-STRIDX
      *    MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *    CALL    "ORCSCWKSRY"        USING
      *                                ORCSCWKSRYAREA
      *                                SPA-AREA
      *                                SPA-KT01
      *                                SPA-SCR-REC
      *    MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    IF      ORCSCWKSRY-ERRCD    NOT =   SPACE
      *        MOVE    ORCSCWKSRY-ERRCD    TO  WRK-ERRCD1
      *    END-IF
      *    IF      ORCSCWKSRY-ERRCD2   NOT =   SPACE
      *        MOVE    ORCSCWKSRY-ERRCD2   TO  WRK-ERRCD3
      *    END-IF
      *
           .
       3103-SAIHEN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院中判定処理
      *****************************************************************
       3109-NYUIN-CHK-SEC              SECTION.
      *
           INITIALIZE                  ORCSCNYUHENAREA
           MOVE    "2"                 TO  ORCSCNYUHEN-KBN
           MOVE    SPA-NAI-SRYYMD      TO  ORCSCNYUHEN-SRYYMD
           CALL    "ORCSCNYUHEN"       USING
                                       SPA-AREA
                                       ORCSCNYUHENAREA
           MOVE    ZERO                TO  FLG-NYUIN
      *    入院中
           IF     (ORCSCNYUHEN-NYUINYMD    NOT =   SPACE )
               IF      ORCSCNYUHEN-NYUDAY (SPA-NAI-SRYDD)
                                           NOT =   ZERO
                   MOVE    1                   TO  FLG-NYUIN
               ELSE
      *            入院中でなく当月に入院がある
                   MOVE    1                   TO  SPA-K02N-NYUINFLG
               END-IF
           END-IF
      *    入退院日
           MOVE    ORCSCNYUHEN-NYUINYMD    TO  SPA-K02N-NYUINYMD
           MOVE    ORCSCNYUHEN-TAIINYMD    TO  SPA-K02N-TAIINYMD
      *    入院期間
           MOVE    ORCSCNYUHEN-NYUIN-DAYG  TO  SPA-K02-NYUIN-AREA
      *    最終退院日
           MOVE    ORCSCNYUHEN-LAST-TAIINYMD
                                           TO  SPA-NAI-LASTTAIIN
           IF      ORCSCNYUHEN-NYUINYMD    =   SPACE
               MOVE    ORCSCNYUHEN-MAE-TAIINYMD
                                           TO  SPA-K02N-TAIINYMD
           END-IF
      *    他医院退院日
           MOVE    ORCSCNYUHEN-TAI-TAIINYMD
                                           TO  SPA-TAI-TAIINYMD
      *    入院中の時、エラーとする
           MOVE    ZERO                TO  SPA-NAI-NYUIN
           IF      FLG-NYUIN           =   1
               MOVE    "6006"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-NAI-NYUIN-ERR
           ELSE
               MOVE    ZERO                TO  SPA-NAI-NYUIN-ERR
           END-IF
      *!!
      *    他医院退院日の算定履歴編集
           IF      FLG-NYUIN           =   ZERO
               PERFORM 1009-SANTEI-HEN-SEC
           END-IF
           .
       3109-NYUIN-CHK-EXT.
           EXIT.
      *
      **DEL --START---
      *****************************************************************
      *    チェック用月内診療行為セット処理
      *****************************************************************
       320-SPA-CHKSET-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-ZAITENTOTAL
           MOVE    SPACE               TO  SPA-ACCT2-SRYCD
           MOVE    ZERO                TO  SPA-ACCT2-ZAINUM
           MOVE    ZERO                TO  SPA-ACCT2-CSYYOURYO
      *
           MOVE    ZERO                TO  FLG-TOUGETU
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX1
               IF     SPA-NAI-RRKYMD (IDX)(1:6)
                                       =   SPA-SRYYMD(1:6)
                   MOVE    1               TO  FLG-TOUGETU
                   MOVE    SPA-GMN-MAX1    TO  IDX
               ELSE
                   IF     SPA-NAI-RRKYMD (IDX)(1:6)
                                           <   SPA-SRYYMD(1:6)
                       MOVE    SPA-GMN-MAX1    TO  IDX
                   END-IF
               END-IF
           END-PERFORM
           IF     (FLG-TOUGETU         =   1   ) OR
                  (SPA-SYORIFLG        =   1   )
      *        当月診療行為チェック用編集、当月累計点数計算
               PERFORM 3201-TOUCHKSET-SEC
           END-IF
      *
      *    禁忌薬剤チェック用編集
      *    IF      SPA-INTERACT-KIKAN  =   ZERO
      *        INITIALIZE                  SPA-KNKYAK-AREA
      *    ELSE
      *        PERFORM 3202-KNKYAKSET-SEC
      *    END-IF
      *
           .
       320-SPA-CHKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック用月内診療行為セット処理
      *****************************************************************
       3201-TOUCHKSET-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-SIN
           MOVE    ZERO                TO  IDX-SIN2
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
      *****MOVE    SPA-SRYKA           TO  ACCT-SRYKA
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    SPA-SRYYMD(7:2)     TO  WRK-TDD
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM UNTIL       (FLG-SRYACCT         =   1       )
      *
      *      包括分入力は対象外
             IF     (ACCT-HKNCOMBI     =   "9999" )
                OR  (ACCT-JIHOKBN      =   "1"    )
               CONTINUE
             ELSE
      *        点数累計
               IF     (ACCT-ZAITEN         =   ZERO )  OR
                      (ACCT-ZAIKAISU       =   ZERO )  OR
                      (ACCT-SRYKBN         =   "95" OR "96")
                   CONTINUE
               ELSE
                   COMPUTE SPA-ZAITENTOTAL  =   SPA-ZAITENTOTAL
                                            +  (ACCT-ZAITEN
                                            *   ACCT-ZAIKAISU )
               END-IF
      *病理診断料のみで使用する為、入力時へ
      *TET     検査で回数があるもの
      *        IF     (ACCT-ZAIKAISU   NOT =   ZERO  )  AND
      *               (ACCT-SRYKBN         =   "60"  )
      *            MOVE    1               TO  FLG-ACCTSYORI
      *            診療行為マスター編集
      *            PERFORM 3201-SRYACT-HEN-SEC
      *TES     END-IF
      *（注射のためにチェックしていたが、現在していないのではずす）
      *復活
      *        当日算定分
               IF     (ACCT-DAY(1 WRK-TDD) >   ZERO  )  AND
                      (ACCT-SRYKBN         =   "33"
                                           OR  "32"  )
                   MOVE    2               TO  FLG-ACCTSYORI
      *            診療行為マスター編集
                   PERFORM 3201-SRYACT-HEN-SEC
               END-IF
      *
             END-IF
      *
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3201-TOUCHKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       3201-SRYACT-HEN-SEC            SECTION.
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
                   IF      SRY-SRYCD (IDX2)(1:1)   =   "8"  OR
                                                       "0"
                       CONTINUE
                   ELSE
                       EVALUATE    FLG-ACCTSYORI
                           WHEN    1
                               PERFORM 32011-SRYACT-HEN1-SEC
                           WHEN    2
                               PERFORM 32012-SRYACT-HEN2-SEC
                       END-EVALUATE
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3201-SRYACT-HEN-EXT.
           EXIT.
      *DEL
      *****************************************************************
      *    診療行為チェック用編集１処理（検査）
      *****************************************************************
       32011-SRYACT-HEN1-SEC           SECTION.
      *
           ADD     1                   TO  IDX-SIN
           IF      IDX-SIN             >   SPA-MAX-LINE
               MOVE    SPA-MAX-LINE            TO  IDX-SIN
           END-IF
           MOVE    ACCT-SRYKBN         TO  SPA-ACCT-SRYKBN
                                                      (IDX-SIN)
           MOVE    SRY-SRYCD (IDX2)    TO  SPA-ACCT-SRYCD
                                                       (IDX-SIN)
      *    点数マスタより、区分番号編集
           MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-CDKBN-KBN       TO  SPA-ACCT-CDKBN-KBN
                                                          (IDX-SIN)
           MOVE    TNS-CDKBN-KBNNUM    TO  SPA-ACCT-CDKBN-KBNNUM
                                                          (IDX-SIN)
      **** MOVE    TNS-CDKBN-SYO       TO  SPA-ACCT-CDKBN-SYO
      *                                                   (IDX-SIN)
      *    MOVE    TNS-CDKBN-BU        TO  SPA-ACCT-CDKBN-BU
      *****                                               (IDX-SIN)
           .
       32011-SRYACT-HEN1-EXT.
           EXIT.
      *DEL
      *****************************************************************
      *    診療行為チェック用編集２処理（点滴）
      *****************************************************************
       32012-SRYACT-HEN2-SEC           SECTION.
      *
      *    点滴
           IF      SRY-SRYSYUKBN       =   "330"  OR
                                           "331"
               IF     (IDX2            =   1     ) AND
                      (SRY-SRYCD (IDX2)(1:1)   =   "1" )
                   MOVE    SRY-SRYCD (IDX2)    TO  SPA-ACCT2-SRYCD
                   MOVE    SRY-ZAINUM          TO  SPA-ACCT2-ZAINUM
               END-IF
      *        点数マスタより編集
               IF      SRY-SRYCD(IDX2)(1:1)    =   "6"
                   MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   COMPUTE SPA-ACCT2-CSYYOURYO =   SPA-ACCT2-CSYYOURYO
                                               +  (TNS-CSYYOURYO
                                               *   SRY-SRYSURYO (IDX2))
               END-IF
           END-IF
      *
      *    静脈注射
           IF     (SRY-SRYSYUKBN       =   "320"  OR
                                           "340" )  AND
                  (SRY-SRYCD (IDX2)(1:1)   =   "1" ) AND
                  (SPA-ACCT2-SRYCD-JYO1    =   SPACE ) AND
                  (SPA-ACCT2-SRYCD-JYO2    =   SPACE )
               MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF     (TNS-DATAKBN         =   "1" )  AND
                      (TNS-CDKBN-KBN       =   "G" )
      *            静脈注射
                   IF    (TNS-CDKBN-KBNNUM =   001 )
                       MOVE    SRY-SRYCD (IDX2)
                                               TO  SPA-ACCT2-SRYCD-JYO1
                   END-IF
      *            中心静脈
                   IF     (TNS-CDKBN-KBNNUM        =   005) AND
                          (TNS-CDKBN-KBNNUM-EDA    =   00 )
                       MOVE    SRY-SRYCD (IDX2)
                                               TO  SPA-ACCT2-SRYCD-JYO2
                   END-IF
               END-IF
           END-IF
      *
           .
       32012-SRYACT-HEN2-EXT.
           EXIT.
      **DEL --END---
      *****************************************************************
      *    算定履歴から他院退院日編集処理
      *****************************************************************
       1009-SANTEI-HEN-SEC             SECTION.
      *
      *    他医院退院日
           MOVE    "099999906"         TO  WRK-SRYCD
      *
           MOVE    ZERO                TO  FLG-TEI-ARI
      **   IF      SPA-SYORIFLG        =   1
      *        PERFORM VARYING    IDX-T    FROM    1   BY  1
      *                UNTIL     (IDX-T    >   SPA-MAX-LINE )  OR
      *                          (SPA-K02-TEI-SRYCD(IDX-T) =   SPACE)
      *            IF      SPA-K02-TEI-SRYCD(IDX-T)    =   WRK-SRYCD
      *                MOVE    1               TO  FLG-TEI-ARI
      *                MOVE    SPA-MAX-LINE    TO  IDX-T
      *            END-IF
      *        END-PERFORM
      **** END-IF
           MOVE    SPACE               TO  WRK-YMD
      *
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    SPA-SRYYMD(1:6)     TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM UNTIL       FLG-SANTEI          =   1
      *        診療日以前の算定日
               IF      SANTEI-SRYYM    =   SPA-SRYYMD(1:6)
                   MOVE    SPA-SRYYMD(7:2) TO  IDX-Y3
      *            訂正で今回算定であれば、前日までを対象
                   IF      FLG-TEI-ARI     =   1
                       COMPUTE IDX-Y3          =    IDX-Y3  -  1
                   END-IF
               ELSE
                   MOVE    31              TO  IDX-Y3
               END-IF
               MOVE    ZERO                TO  WRK-DD-1
               PERFORM VARYING     IDX-Y2  FROM    1  BY  1
                       UNTIL       IDX-Y2      >   IDX-Y3
                   IF      SANTEI-DAY (IDX-Y2) >   ZERO
                       MOVE    IDX-Y2          TO  WRK-DD-1
                   END-IF
               END-PERFORM
               IF      WRK-DD-1            >   ZERO
                   MOVE    SANTEI-SRYYM        TO  WRK-YMD(1:6)
                   MOVE    WRK-DD-1            TO  WRK-DD
                   MOVE    1                   TO  FLG-SANTEI
               END-IF
               IF      FLG-SANTEI          =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-YMD         NOT =   SPACE
               IF      WRK-YMD         >   SPA-TAI-TAIINYMD
                   MOVE    WRK-YMD     TO  SPA-TAI-TAIINYMD
               END-IF
           END-IF
           .
       1009-SANTEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック用編集処理
      *****************************************************************
      * 3202-KNKYAKSET-SEC              SECTION.
      *
      *    INITIALIZE                  ORCSCKNKCHKAREA
      *    MOVE    "1"                 TO  ORCSCKNKCHK-KBN 
      *     CALL    "ORCSCKTKNKCHK"     USING
      *                                 ORCSCKNKCHKAREA
      *                                 SPA-AREA
      *                                 SPA-KT01
      *                                 SPA-SCR-REC
      *
      *     .
      * 3202-KNKYAKSET-EXT.
      *     EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        患者取消
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 222-PTNUMCLEAR-SEC
      *        前回の患者番号
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 230-MAEKPTNUM-SEC
      *        算定履歴更新（ＫＴ０３）
               WHEN    "CLICKED"       ALSO    "B10S"
                   PERFORM 430-SANTEIRRK-SEC
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02S"
                   PERFORM 220-CLEAR-SEC
      *        収納登録（Ｓ０２）
               WHEN    "CLICKED"       ALSO    "B08S"
                   PERFORM 460-SYUNOU-SEC
      *        会計カード（Ｊ０２）
               WHEN    "CLICKED"       ALSO    "B09S"
                   PERFORM 450-KAIKEI-SEC
      *        病名登録　（Ｃ０２）
               WHEN    "CLICKED"       ALSO    "B05S"
                   PERFORM 440-BYOTOROKU-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 450-MAEGMN-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 460-ATOGMN-SEC
      *        ＤＯ処理
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 480-DOSYORI-SEC
      *        氏名検索（Ｐ９７）
               WHEN    "CLICKED"       ALSO    "B09"
                   MOVE    SPACE              TO  WRK-P97-NAME
                   PERFORM 470-P97-KENSAKU-SEC
      *        算定（包括算定チェック）
               WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 4911-HKTSANTEI-CHEK-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 4110-KAKUTEI-SEC
      **       登録
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE    SPACE           TO  WRK-PUTTYPE
                   PERFORM 4100-TOROKU-SEC
      *        院外院内
               WHEN    "CLICKED"       ALSO    "INGAI"
                   PERFORM 4101-INGAI-SYORI-SEC
      *        前受診
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 4109-B05-SYORI-SEC
      *        後受診
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 4109-B08-SYORI-SEC
      *        前月
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 4106-B06S-SYORI-SEC
      *        次月
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 4107-B07S-SYORI-SEC
      *        診察料
               WHEN    "CLICKED"       ALSO    "B04S"
                   PERFORM 4104-B04S-SYORI-SEC
      *        一括削除
               WHEN    "CLICKED"       ALSO    "B01S"
                   PERFORM 2101-ALLDELETE-SEC
      *        受診日
               WHEN    "CLICKED"       ALSO    ANY
                   IF      MCP-WIDGET(1:2)     =   "BD"
                       PERFORM 4104-BDAY-SYORI-SEC
                   END-IF
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力項目の決定
           IF      MCP-EVENT           =   "ACTIVATE"   OR
                                           "ACTIVATE2"
               IF     (MCP-WIDGET(1:07)    =   "INPUTCD"  )  OR
                      (MCP-WIDGET(1:6)     =   "MEISYO"   )
                   PERFORM 2001-INPUT-SET-SEC
                   IF      WRK-IDX2        =   ZERO
                       GO      TO      200-ENTRY-EXT
                   END-IF 
               ELSE
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET
               END-IF
           ELSE
               MOVE    MCP-WIDGET          TO  WRK-WIDGET
           END-IF
      *
           EVALUATE    MCP-EVENT       ALSO    WRK-WIDGET
      *        患者番号入力
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 41010-PTNUM-SYORI-SEC
      *        保険番号入力
               WHEN    "ACTIVATE"      ALSO    "HKNCOMBI"
                   PERFORM 41012-HKNCOMBI-SYORI-SEC
      *        診療年月入力
               WHEN    "ACTIVATE"      ALSO    "SRYYM"
                   PERFORM 41017-SRYYM-SYORI-SEC
      *        診療日入力
               WHEN    "ACTIVATE"      ALSO    "SRYDD"
                   PERFORM 41017-SRYDD-SYORI-SEC
      *        診療科入力
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 41013-SRYKAI-SYORI-SEC
      *        ドクター
               WHEN    "ACTIVATE"      ALSO    "DRNAME"
                   PERFORM 41013-DRNAME-SYORI-SEC
      *        入力コード
               WHEN    "ACTIVATE"      ALSO    "INPUTCD"
                   PERFORM 41011-KOUI-ALLHEN-SEC
      *        名称
               WHEN    "ACTIVATE"      ALSO    "MEISYO"
                    PERFORM 41011-KOUI-ALLHEN-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目の決定
      *****************************************************************
       2001-INPUT-SET-SEC               SECTION.
      *
           MOVE    SPACE               TO  FLG-TOROKU
      *    入力行NOの設定
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-WIDGET
           IF      MCP-WIDGET(1:07)    =   "INPUTCD"
               MOVE    MCP-WIDGET(8:2)     TO  WRK-IDX2
               MOVE    MCP-WIDGET(1:7)     TO  WRK-WIDGET
           END-IF
           IF      MCP-WIDGET(1:6)     =   "MEISYO"
               MOVE    MCP-WIDGET(7:2)     TO  WRK-IDX2
               MOVE    MCP-WIDGET(1:6)     TO  WRK-WIDGET
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX
                   MOVE    SPA-MAX-LINE            TO  IDX
               END-IF
           END-PERFORM
      *
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX         =   ZERO
               COMPUTE SPA-GMN-IDX     =   (SPA-GMN-PAGE   -   1)
                                       *   MAX-GMN
                                       +   WRK-IDX2
           END-IF
      *
           .
       2001-INPUT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号入力処理
      *****************************************************************
       41010-PTNUM-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-GMN-PTID
           IF      SPA-PTID        NOT =   ZERO
      *        ワーク診療行為クリア
               PERFORM 4900-WKSRYACT-CLEAR-SEC
               MOVE    SPA-PTID        TO  WRK-GMN-PTID
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    1                   TO  FLG-GMN-INIT
      *    ＳＰＡクリア処理
           INITIALIZE                  SPA-SCR-REC
           INITIALIZE                  SPA-KT01
           INITIALIZE                  SPA-K01SET-AREA
           INITIALIZE                  SPA-K02SET-AREA
           INITIALIZE                  SPA-SYORIFLG
           INITIALIZE                  SPA-DENPNUM
           INITIALIZE                  SPA-CONTFLG
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
           MOVE    SPA-SRYYMD(7:2)     TO  SPA-GMN-SRYDD
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
      *
           MOVE    KT01-PTNUM          TO  SPA-GMN-PTNUM
      *
           EVALUATE    TRUE
               WHEN   (SPA-GMN-PTNUM           =   SPACE)
                   MOVE    WRK-GMN-PTID        TO  SPA-GMN-PTID
      *        空白は患者取消と同様
                   PERFORM 2221-PTNUMCLEAR-SYORI-SEC
               WHEN    OTHER
      *        患者決定処理
                   PERFORM 410101-PTNUM-IN-SYORI-SEC
           END-EVALUATE
      *
           .
       41010-PTNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号検索処理
      *****************************************************************
       410101-PTNUM-IN-SYORI-SEC         SECTION.
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           EVALUATE    SPTNUM-RC
               WHEN    10
               WHEN    99
      *            患者番号なし
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
                   MOVE    "1003"              TO  SPA-ERRCD
               WHEN    20
      *            氏名検索へ
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
                   MOVE    SPTNUM-PTNUM        TO  WRK-P97-NAME
                   PERFORM 470-P97-KENSAKU-SEC
               WHEN    OTHER
      *            患者番号決定
                   MOVE    SPTNUM-PTID         TO  SPA-PTID
                   MOVE    SPTNUM-PTNUM        TO  SPA-PTNUM
                   MOVE    SPA-PTID            TO  SPA-GMN-PTID
                   MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
      *            排他制御処理
                   PERFORM 990-LOCK-IN-SEC
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 410101-PTNUM-SYOKI-SEC
                   END-IF
           END-EVALUATE
           .
      *
       410101-PTNUM-IN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *   患者番号決定初期処理
      *****************************************************************
       410101-PTNUM-SYOKI-SEC         SECTION.
      *
      *    初期ＳＰＡ編集処理（システム日付を診療日とする）
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SYSYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  LNK-SC95-SRYYMDG
           INITIALIZE                  SPA-DENPNUM
           INITIALIZE                  SPA-SYORIFLG
           INITIALIZE                  SPA-LAST-PTID
           INITIALIZE                  SPA-LAST-PTNUM
           INITIALIZE                  SPA-KT01
           INITIALIZE                  SPA-SCR-REC
      *    診療日に変更があったらＳＰＡを指定する
           IF      SPA-SRYYMD          =   SPA-SYSYMD
               MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
               MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
      *        院外処方区分
               MOVE    SPA-SYOHOKBN        TO  SPA-INGAIKBN
           ELSE
               MOVE    "1"                 TO  LNK-SC95-KBN
               CALL    "ORCSCKT95"         USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-KT01
           END-IF
      *
           MOVE    "2"                 TO  LNK-SC95-KBN
           INITIALIZE                  SPA-SRYKA
           CALL    "ORCSCKT95"         USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-KT01
      *    患者決定共編集処理
           MOVE    1               TO  FLG-CHKSET
           PERFORM 4102-PTNUM-HENSYU-SEC
      *
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
      *        システム管理よりカーソル指定
               EVALUATE    SPA-GAI-CURSOR-SET
               WHEN    "1"
      *            保険組合せ
                   MOVE    05              TO  SPA-GMN-CUR
               WHEN    "2"
      *            診療科
                   MOVE    06              TO  SPA-GMN-CUR
               END-EVALUATE
               IF      LNK-WKSRYACT-MAX        >   ZERO
                   MOVE    01              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       410101-PTNUM-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者決定共編集処理
      *****************************************************************
       4102-PTNUM-HENSYU-SEC         SECTION.
      *
      *    スパ共通編集処理
           PERFORM 310-SPA-SET-SEC
      *    誕生日以前の時、エラー
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
               IF      SPA-SRYYMD          <   SPA-BIRTHDAY
                    MOVE    "1011"         TO  SPA-ERRCD
                    MOVE    7              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    保険番号が未確定の時、エラー
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
               IF      SPA-HKNCOMBINUM     =   SPACE
                   MOVE    "1006"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-HKN-ERR
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
               CONTINUE
           ELSE
               GO      TO      4102-PTNUM-HENSYU-EXT
           END-IF
      *!!!
      *
      *    当月算定履歴から包括判定
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSCKT10SAREA
           MOVE    "5"                 TO  ORCSCKT10S-SYORI
           CALL    "ORCSCKT10S"        USING
                                       ORCSCKT10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-KT01
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    受診履歴変更確認
           MOVE    1                   TO  SPA-GMN-CUR
           PERFORM 41013-JYURRK-CHK-SEC
           IF      FLG-RRK-ARI         =   ZERO
               MOVE    ZERO                TO  SPA-NAI-RRK-ERR
           ELSE
               MOVE    1                   TO  SPA-NAI-RRK-ERR
      *        月末まで算定済み
               IF      SPA-NAI-LSTDD       =   SPA-SRYLST-DD
                   MOVE    2                   TO  SPA-NAI-RRK-ERR
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "6002"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "6008"              TO  SPA-ERRCD
                   END-IF
      *            診療日変更
                   IF      SPA-NAI-LSTDD       >=  SPA-GMN-SRYDD
                       COMPUTE SPA-NAI-SRYDD       =   SPA-NAI-LSTDD
                                                   +   1
                       MOVE    SPA-NAI-SRYDD       TO  SPA-GMN-SRYDD
                       MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
                   END-IF
               END-IF
           END-IF
      *
           .
       4102-PTNUM-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    診療年月日入力処理
      *****************************************************************
       41017-SRYYM-SYORI-SEC          SECTION.
      *
           MOVE    7                   TO  SPA-GMN-CUR
           MOVE    ZERO                TO  SPA-NAI-ROUSTR
           MOVE    ZERO                TO  FLG-SRYYM-HEN
      *
           MOVE    KT01-SRYYM          TO  SPA-GMN-SRYYM
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-SRYYM       TO  SGDAY-INDAY
           MOVE    "1"                 TO  SGDAY-INTYPE
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-SRYYM
               IF      SGDAY-SDAY (1:6)    NOT =   SPA-NAI-SRYYM
                   MOVE    1                   TO  FLG-SRYYM-HEN
               END-IF
               MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI-SRYYM
           ELSE
               MOVE    "1010"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-SRYYM       <   "200804"
                   MOVE    "6004"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        診療日の末日を求める
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    SPA-NAI-SRYYM       TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
               IF      STS-DAY-RC1         =   ZERO
                   MOVE    LNK-DAY7-KOYOMI(7:2)    TO  SPA-SRYLST-DD
               ELSE
                   MOVE    31                  TO  SPA-SRYLST-DD
               END-IF
      *        日設定
               PERFORM 41017-SRYDD-SYORI-SEC
           END-IF
           .
       41017-SRYYM-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療日入力処理
      *****************************************************************
       41017-SRYDD-SYORI-SEC          SECTION.
      *
           MOVE    8                   TO  SPA-GMN-CUR
      *
           MOVE    SPA-NAI-SRYDD       TO  WRK-MAE-SRYDD
      *
           MOVE    KT01-SRYDD          TO  SPA-GMN-SRYDD
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-SRYDD       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "1010"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  SPA-GMN-SRYDD
           END-IF
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-NAI-SRYYM(1:6)  TO  SGDAY-INDAY(1:6)
           MOVE    SPA-GMN-SRYDD       TO  SGDAY-INDAY(7:2)
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               CONTINUE
           ELSE
               MOVE    "1010"              TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD               =   SPACE
      *        患者の確定してない時
               IF      SPA-PTID            =   ZERO
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYMD
                   MOVE    ZERO                TO  SPA-GMN-CUR
                   GO      TO      41017-SRYDD-SYORI-EXT
               END-IF
      *        誕生日以前の時、エラー
               IF      SGDAY-SDAY           <   SPA-BIRTHDAY
                    MOVE    "1011"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    最終受診日のチェック
           IF    (SPA-ERRCD            =   SPACE )  AND
                 (SPA-KIDCD            =   SPACE )
               IF     (SPA-NAI-SRYYM       =   SPA-SRYYMD(1:6) )
                 AND  (SPA-NAI-LSTDD       >=  SPA-GMN-SRYDD   )
                   MOVE    WRK-MAE-SRYDD   TO  SPA-GMN-SRYDD
                   MOVE    WRK-MAE-SRYDD   TO  SPA-NAI-SRYDD
                   MOVE    "6007"          TO  SPA-ERRCD
               END-IF
           END-IF
      *    入院中判定
           IF    (SPA-ERRCD            =   SPACE )  AND
                 (SPA-KIDCD            =   SPACE )
               MOVE    SPA-GMN-SRYDD       TO  IDD2
               IF     (SPA-NAI-SRYYM       =   SPA-SRYYMD(1:6) )
                 AND  (SPA-K02-NYUDAY (IDD2)
                                       NOT =   ZERO   )
                   MOVE    IDD2                TO  SPA-ERRMSG2
                   MOVE    "6001"              TO  SPA-ERRCD
                   MOVE    WRK-MAE-SRYDD       TO  SPA-GMN-SRYDD
                   MOVE    WRK-MAE-SRYDD       TO  SPA-NAI-SRYDD
               END-IF
           END-IF
      *
           IF    (SPA-ERRCD            =   SPACE )  AND
                 (SPA-KIDCD            =   SPACE )
      *        変更のない時、以降の処理はそのまま
               IF     (SGDAY-SDAY          =   SPA-NAI-SRYYMD )  AND
                      (FLG-SRYYM-HEN       =   ZERO           )
                    MOVE    1                  TO  SPA-GMN-CUR
               ELSE
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYMD
      *            診療日確定後、編集
                   PERFORM 410171-SRYYMD-OK-SEC
               END-IF
           END-IF
           .
       41017-SRYDD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日確定後、編集処理
      *****************************************************************
       410171-SRYYMD-OK-SEC         SECTION.
      *
           IF     (SPA-NAI-SRYYM       =   SPA-SRYYMD(1:6) )
             AND  (SPA-NAI-LSTDD       >=  SPA-GMN-SRYDD   )
               MOVE    "6007"          TO  SPA-ERRCD
               GO      TO  410171-SRYYMD-OK-EXT
           END-IF
      *
      *
           IF      SPA-NAI-SRYYM       =   SPA-SRYYMD(1:6)
               MOVE    SPACE               TO  SPA-KIDCD
               IF      SPA-NAI-RRKDAY-G    =   ZERO
                   CONTINUE
               ELSE
                   MOVE    SPA-NAI-SRYDD      TO  WRK-DAY
                   PERFORM 6001-DAYCHEK-SEC
                   MOVE    1                  TO  SPA-GMN-CUR
               END-IF
      *        入院判定
               IF      SPA-NAI-NYUIN-ERR   =   1
                   PERFORM 410171-SRYYMD-INIT-SEC
               ELSE
      *            診療日入院中
                   IF      SPA-K02-NYUDAY (SPA-NAI-SRYDD)
                                           NOT =   ZERO
                      MOVE    1                TO  SPA-NAI-NYUIN-ERR
                   END-IF
               END-IF
               IF      SPA-GMN-MAX         =   ZERO
                   IF      SPA-NAI-RRKDAY-G    =   ZERO
                       PERFORM 410171-SRYYMD-INIT-SEC
                    ELSE
                       PERFORM 4101712-HKNCOMBI-CHK-SEC
                   END-IF
               END-IF
           ELSE
               PERFORM 410171-SRYYMD-INIT-SEC
           END-IF
           .
       410171-SRYYMD-OK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日と保険組合せチェック処理
      *****************************************************************
       4101712-HKNCOMBI-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBICHK
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-HKN-MAX
               IF      SPA-GMN-HKNCOMBINUM =   SPA-GMN-THKNCOMBINUM
                                                         (IDY)
                   IF     (SPA-NAI-TEKSTYMD (IDY)  <=   SPA-NAI-SRYYMD)
                   AND    (SPA-NAI-TEKEDYMD (IDY)  >=   SPA-NAI-SRYYMD)
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-HKNCOMBICHK
                       MOVE    SPA-HKN-MAX     TO  IDY
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-HKNCOMBICHK     =   1
               MOVE    WRK-MAE-SRYDD       TO  SPA-NAI-SRYDD
               MOVE    WRK-MAE-SRYDD       TO  SPA-GMN-SRYDD
               MOVE    SPA-NAI-SRYDD       TO  WRK-DAY
               PERFORM 6001-DAYCHEK-SEC
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "6010"              TO  SPA-ERRCD
           END-IF
           .
       4101712-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日確定後、編集処理
      *****************************************************************
       410171-SRYYMD-INIT-SEC         SECTION.
      *
           PERFORM 4900-WKSRYACT-CLEAR-SEC
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-NAI-SRYYMD      TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
           MOVE    SGDAY-OTDAY         TO  LNK-SC95-SRYYMDG
           INITIALIZE                  SPA-DENPNUM
           INITIALIZE                  SPA-SYORIFLG
           INITIALIZE                  SPA-KT01
           INITIALIZE                  SPA-SCR-REC
           INITIALIZE                  SPA-K01SET-AREA
           INITIALIZE                  SPA-K02SET-AREA
           MOVE    "1"                 TO  LNK-SC95-KBN
           CALL    "ORCSCKT95"         USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-KT01
      *
           MOVE    "2"                 TO  LNK-SC95-KBN
           CALL    "ORCSCKT95"         USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-KT01
      *
      *    患者決定共編集処理
           MOVE    1               TO  FLG-CHKSET
           PERFORM 4102-PTNUM-HENSYU-SEC
      *
           .
       410171-SRYYMD-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号入力処理
      *****************************************************************
       41012-HKNCOMBI-SYORI-SEC         SECTION.
      *
           MOVE    5                   TO  SPA-GMN-CUR
      *
      *    保険番号入力
           IF    ( KT01-HKNCOMBI        =   SPA-GMN-HKNCOMBI-G)  OR
                 ( KT01-PTNUM           =   SPACE             )
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      41012-HKNCOMBI-SYORI-EXT
           END-IF
      *
           MOVE    SPA-GMN-HKNCOMBI-G  TO  SPA-MAE-HKNCOMBI-G
           MOVE    SPA-HKNKBN          TO  SPA-MAE-HKNKBN
           MOVE    SPA-NAI-ROUJIN      TO  SPA-MAE-ROUJIN
      *
           MOVE    KT01-HKNCOMBI       TO  SPA-GMN-HKNCOMBI-G
           PERFORM 41013-JYURRK-CHK-SEC
           MOVE    ZERO                TO  SPA-NAI-RRK-ERR
           IF      FLG-RRK-ARI         =   ZERO
               MOVE    ZERO                TO  FLG-CHK2
      *        保険組合せ変更
               PERFORM 41012-HKNCOMBI-CHANGE-SEC
           ELSE
      *        他受診履歴への変更確認
      *!!!!    MOVE    "0105"              TO  SPA-KIDCD
      *        月末まで算定済み
               IF      SPA-NAI-LSTDD       =   SPA-SRYLST-DD
                   MOVE    2                   TO  SPA-NAI-RRK-ERR
                   MOVE    "6007"              TO  SPA-ERRCD
               ELSE
                   MOVE    1                   TO  SPA-NAI-RRK-ERR
                   IF     (SPA-GMN-MAX         =   ZERO )
                     AND  (SPA-NAI-RRKDAY-G    =   ZERO )
                       MOVE    "6008"              TO  SPA-ERRCD
                       PERFORM 410141-CHANGE-INIT-SEC
      *                保険組合
                       PERFORM 3101-HKNCOMBI-HEN-SEC
                   ELSE
                       MOVE    "0105"              TO  SPA-KIDCD
                   END-IF
               END-IF
           END-IF
           .
       41012-HKNCOMBI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号変更チェック処理
      *****************************************************************
       41013-JYURRK-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-RRK-ARI
           MOVE    ZERO                TO  SPA-NAI-LSTDD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX1
               IF      (SPA-GMN-SRYKA      =   SPA-NAI-RRK-SRYKA (IDX))
                   AND (SPA-GMN-HKNCOMBINUM    =   SPA-NAI-RRK-HKNCOMBI
                                                                 (IDX))
                   MOVE    1               TO  FLG-RRK-ARI
                   IF      SPA-NAI-LSTDD   <   SPA-NAI-RRKYMD
                                                       (IDX)(7:2)
                       MOVE   SPA-NAI-RRKYMD(IDX)(7:2) TO  SPA-NAI-LSTDD
                   END-IF
               END-IF
           END-PERFORM
           .
       41013-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号変更入力処理
      *****************************************************************
       41012-HKNCOMBI-CHANGE-SEC         SECTION.
      *
           IF    ((SPA-RJN-ERR         =   ZERO )  AND
                  (SPA-HKN-ERR         =   ZERO
                                       OR  4    ) )
              OR ( SPA-MAE-HKNCOMBINUM =   ZERO  )
               MOVE    ZERO                TO  FLG-HKNCHK
           ELSE
               MOVE    1                   TO  FLG-HKNCHK
           END-IF
      *    保険組合
           PERFORM 3101-HKNCOMBI-HEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )
               OR (SPA-KIDCD       NOT =   SPACE )
               GO      TO      41012-HKNCOMBI-CHANGE-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-GMN-CUR
      *
      *    保険区分が変更された時、再編集を行う
           IF     (SPA-HKNKBN          =   SPA-MAE-HKNKBN)  AND
                  (SPA-NAI-ROUJIN      =   SPA-MAE-ROUJIN)
      *            労災の時、初回算定日を再編集する
             AND  (SPA-HKNKBN          =   ZERO)
      *           変更前がエラーでないこと
             AND  (FLG-HKNCHK          =   ZERO )
               CONTINUE
           ELSE
               IF     (SPA-GMN-MAX         >   ZERO )
                   PERFORM 410121-HKNCOMBI-SAI-SEC
               END-IF
           END-IF
      *
           .
       41012-HKNCOMBI-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ変更確認処理
      *****************************************************************
       41014-HKN-CHANGE-SYORI-SEC         SECTION.
      *
           IF      SPA-KID1-FLG        =   "OK"
      *        クリア
               PERFORM 410141-CHANGE-INIT-SEC
      *        保険組合編集
               PERFORM 3101-HKNCOMBI-HEN-SEC
           ELSE
               MOVE    SPA-MAE-HKNCOMBI-G
                                           TO  SPA-GMN-HKNCOMBI-G
               MOVE    SPA-MAE-HKNKBN      TO  SPA-HKNKBN
               MOVE    SPA-MAE-ROUJIN      TO  SPA-NAI-ROUJIN
               PERFORM 41013-JYURRK-CHK-SEC
               IF      FLG-RRK-ARI         =   ZERO
                   MOVE    ZERO                TO  SPA-NAI-RRK-ERR
               ELSE
                   MOVE    1                   TO  SPA-NAI-RRK-ERR
               END-IF
           END-IF
           .
       41014-HKN-CHANGE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分入力変換処理
      *****************************************************************
       410122-HOUKATU-HKNCOMBI-SEC         SECTION.
      *
      *    包括まとめの時、フラグクリア
      *    寝たきり老人在宅総合診療科
           MOVE    ZERO                TO  SPA-NETAKIRI-ARI
           MOVE    ZERO                TO  SPA-NETAKIRI-ARI2
      *    在宅末期医療総合診察料
           MOVE    ZERO                TO  SPA-ZAIMATU-ARI
           MOVE    ZERO                TO  SPA-ZAIMATU-ARI2
      *    生活習慣病
           MOVE    ZERO                TO  SPA-SEIKATU-ARI
           MOVE    ZERO                TO  SPA-SEIKATU-ARI2
      *    慢性疼痛疾病管理料
           MOVE    ZERO                TO  SPA-TOKUTEI-KNJ2
      *    痴呆患者在宅指導管理料（外来加算付加算定付加）
           MOVE    ZERO                TO  SPA-GAIRAI-NASI
      *    消炎鎮痛等処置
           MOVE    ZERO                TO  SPA-SYOEN-CNT
      *
           PERFORM 410121-HKNCOMBI-SAI-SEC
           .
       410122-HOUKATU-HKNCOMBI-EXT.
           EXIT.
      *****************************************************************
      *    保険区分変換処理
      *****************************************************************
       410121-HKNCOMBI-SAI-SEC         SECTION.
      *
      *    MOVE    SPA-WKSRY-HKNCOMBI  TO  WRK-WKSRY-HKNCOMBI
      *    初期ＳＰＡ編集処理
      *    INITIALIZE                  ORCSC95AREA
      *    MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
      *    MOVE    "7"                 TO  LNK-SC95-KBN
      *    CALL    "ORCSCKT95"         USING
      *                                MCPAREA
      *                                ORCSC95AREA
      *                                SPA-AREA
      *                                SPA-K01KYOUTU
      *                                SPA-KT01
      *    MOVE    WRK-WKSRY-HKNCOMBI  TO  SPA-WKSRY-HKNCOMBI
      *
      *    訂正でなく、包括保険に変更した場合、メッセージ表示
      *    IF      SPA-GMN-HKNCOMBINUM     =   "9999"
      *        IF     (SPA-GMN-MAX         >   ZERO )
      *            MOVE    "0190"              TO  SPA-KIDCD
      *            GO      TO      410121-HKNCOMBI-SAI-EXT
      *        END-IF
      *    END-IF
      *
      *    保険区分が変更された時、再編集を行う
           IF     (SPA-HKNKBN          =   SPA-MAE-HKNKBN  )  AND
                  (SPA-HKNKBN          =   ZERO            )  AND
                  (SPA-NAI-ROUJIN      =   SPA-MAE-ROUJIN)
             AND  (SPA-MAE-HKNCOMBINUM NOT =   "9999"  )
             AND  (FLG-HKNCHK          =   ZERO )
               CONTINUE
           ELSE
               PERFORM 410131-SINSATUHEN-SEC
           END-IF
      *
           .
       410121-HKNCOMBI-SAI-EXT.
           EXIT.
      *****************************************************************
      *    診察料再編集処理
      *****************************************************************
       410131-SINSATUHEN-SEC         SECTION.
      *
      *    老人区分が変更された時、チェックフラグをクリアする
           IF     (SPA-NAI-ROUJIN  NOT =   SPA-MAE-ROUJIN) OR
                  (SPA-HKNKBN      NOT =   SPA-MAE-HKNKBN )
               PERFORM 4101311-ROUJINCHK-SEC
           END-IF
      *
           IF     (SPA-HKNKBN      NOT =   SPA-MAE-HKNKBN ) OR
                  (SPA-HKNKBN      NOT =   ZERO           )
      *        リハビリ情報クリア
               INITIALIZE              SPA-RIGAKU-NCNT-AREA
           END-IF
      *
      *    診療行為、計計算処理、編集
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           .
       410131-SINSATUHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックフラグクリア処理
      *****************************************************************
       4101311-ROUJINCHK-SEC         SECTION.
      *
      *    老人区分が変更された時、チェックフラグをクリアする
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               MOVE    SPACE               TO  SPA-COM-CHKFLG (IDX)
               MOVE    SPACE               TO  SPA-COM-HCHKFLG (IDX)
               MOVE    SPACE               TO  SPA-MAE-INPUTCD(IDX)
           END-PERFORM
           .
       4101311-ROUJINCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       410132-SAIHENSYU-SEC         SECTION.
      *
      *    中間ワーク診療行為データクリア
           PERFORM 4900-WKSRYACT-CLEAR-SEC
      *
      *    患者決定共編集処理（チェック用月内診療行為セットなし）
           MOVE    ZERO                TO  FLG-CHKSET
           PERFORM 4102-PTNUM-HENSYU-SEC
      *
           .
       410132-SAIHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       41013-SRYKAI-SYORI-SEC         SECTION.
      *
           MOVE    6                   TO  SPA-GMN-CUR
      *
      *    診療科変更あり
           IF      KT01-SRYKA          =   SPA-GMN-SRYKA-G
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
           IF      SPA-PTID            =   ZERO
               MOVE    ZERO                TO  SPA-GMN-CUR
               MOVE    KT01-SRYKA          TO  SPA-GMN-SRYKA-G
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
      *
           MOVE    SPA-GMN-SRYKA-G     TO  SPA-MAE-SRYKA-G
           MOVE    KT01-SRYKA          TO  SPA-GMN-SRYKA-G
      *    診療科
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKALST (IDX)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX   TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *    科が無い時、前の科に戻す
           IF      FLG-CHK             =   ZERO
               IF      SPA-MAE-SRYKA       =   SPACE
                   MOVE    SPA-GMN-SRYKALST (1)
                                           TO  SPA-GMN-SRYKA-G
               ELSE
                   MOVE    SPA-MAE-SRYKA-G TO  SPA-GMN-SRYKA-G
                   GO      TO      41013-SRYKAI-SYORI-EXT
               END-IF
           END-IF
      *
           IF     (SPA-GMN-MAX         >   ZERO )
             OR   (SPA-NAI-RRKDAY-G    >   ZERO )
      *        診療入力済みは、保険組合せ変更なし
               MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
               MOVE    SPA-GMN-SRYKAMEI    TO  SPA-SRYKAMEI
               PERFORM 41013-SRYKA-CHANGE-SEC
           ELSE
      *        保険組合せ変更
               PERFORM 41013-SRYKA-CHANGE-HKN-SEC
           END-IF
      *
           .
       41013-SRYKAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科変換処理
      *****************************************************************
       41013-SRYKA-CHANGE-HKN-SEC         SECTION.
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-SRYKAMEI
      *
      *    保険組合せ変更判定
           MOVE    SPA-KT01            TO  HZN-SPA-KT01
           MOVE    SPA-K01KYOUTU       TO  HZN-SPA-K01KYOUTU
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           MOVE    SPACE               TO  WRK-SPA-HKNCOMBINUM
           MOVE    SPACE               TO  WRK-SPA-DRCD
           MOVE    SPACE               TO  HZN-SPA-DRCD-NAME
      *    保険組合せ・ドクター初期編集
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
           MOVE    "K"                 TO  LNK-SC95-KBN
           CALL    "ORCSCKT95"         USING
                                       MCPAREA
                                       ORCSC95AREA
                                       WRK-SPA-AREA
                                       HZN-SPA-K01KYOUTU
                                       HZN-SPA-KT01
           IF      SPA-HKNCOMBINUM     =   WRK-SPA-HKNCOMBINUM
      *        保険変更がない時、処理を行う
               MOVE    SPACE               TO  SPA-KID1-FLG
               MOVE    WRK-SPA-DRCD        TO  SPA-DRCD
               MOVE    HZN-SPA-DRCD-NAME   TO  SPA-DRCD-NAME
      *        変更後の保険組合せでチェック
               PERFORM 41013-SRYKA-CHANGE-SEC
           ELSE
               MOVE    "0138"              TO  SPA-KIDCD
           END-IF
      *
      *    変更後の保険組合せでチェック
      *    PERFORM 41013-SRYKA-CHANGE-SEC
      *
           .
       41013-SRYKA-CHANGE-HKN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科変換処理
      *****************************************************************
       41013-SRYKA-CHANGE-HKN02-SEC         SECTION.
      *
           IF      SPA-KID1-FLG        =   "OK"
               MOVE    SPACE               TO  SPA-HKNCOMBINUM
           END-IF
           MOVE    SPACE               TO  SPA-DRCD
           MOVE    SPACE               TO  SPA-DRCD-NAME
      *    保険組合せ・ドクター初期編集
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
           MOVE    "K"                 TO  LNK-SC95-KBN
           CALL    "ORCSCKT95"         USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-KT01
      *
      *    変更後の保険組合せでチェック
           PERFORM 41013-SRYKA-CHANGE-SEC
           .
       41013-SRYKA-CHANGE-HKN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科変換処理
      *****************************************************************
       41013-SRYKA-CHANGE-SEC         SECTION.
      *
           PERFORM 41013-JYURRK-CHK-SEC
           MOVE    ZERO                TO  SPA-NAI-RRK-ERR
           IF      FLG-RRK-ARI         =   ZERO
               CONTINUE
           ELSE
      *        月末まで算定済み
               IF      SPA-NAI-LSTDD       =   SPA-SRYLST-DD
                   MOVE    2                   TO  SPA-NAI-RRK-ERR
                   MOVE    "6007"              TO  SPA-ERRCD
               ELSE
                   MOVE    1                   TO  SPA-NAI-RRK-ERR
                   IF     (SPA-GMN-MAX         =   ZERO )
                     AND  (SPA-NAI-RRKDAY-G    =   ZERO )
                       MOVE    "6008"              TO  SPA-ERRCD
                       PERFORM 410141-CHANGE-INIT-SEC
                   ELSE
                       MOVE    "0123"              TO  SPA-KIDCD
                   END-IF
               END-IF
           END-IF
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           .
       41013-SRYKA-CHANGE-EXT.
           EXIT.
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       41014-CHANGE-SYORI-SEC         SECTION.
      *
      *    変更
           IF      SPA-KID1-FLG        =   "OK"
      *        初期クリア
               PERFORM 410141-CHANGE-INIT-SEC
           ELSE
               MOVE    SPA-MAE-SRYKA-G     TO  SPA-GMN-SRYKA-G
               PERFORM 41013-JYURRK-CHK-SEC
               IF      FLG-RRK-ARI         =   ZERO
                   MOVE    ZERO                TO  SPA-NAI-RRK-ERR
               ELSE
                   IF      SPA-NAI-LSTDD       =   SPA-SRYLST-DD
                       MOVE    2                   TO  SPA-NAI-RRK-ERR
                   ELSE
                       MOVE    1                   TO  SPA-NAI-RRK-ERR
                   END-IF
               END-IF
           END-IF
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-GMN-PAGE
           .
       41014-CHANGE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    変更初期処理
      *****************************************************************
       410141-CHANGE-INIT-SEC         SECTION.
      *
      *    中間データクリア
           PERFORM 4900-WKSRYACT-CLEAR-SEC
      *
           INITIALIZE                  SPA-SCR-REC
           INITIALIZE                  SPA-GMN-MAX
           INITIALIZE                  SPA-NAI-RRKDAY-G
           MOVE    ZERO                TO  SPA-GMN-GOKEI
           IF      SPA-NAI-LSTDD       >=  SPA-GMN-SRYDD
               COMPUTE SPA-NAI-SRYDD       =   SPA-NAI-LSTDD
                                           +   1
               MOVE    SPA-NAI-SRYDD       TO  SPA-GMN-SRYDD
               MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
           END-IF
           .
       410141-CHANGE-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター入力処理
      *****************************************************************
       41013-DRNAME-SYORI-SEC         SECTION.
      *
           MOVE    KT01-DRNAME         TO  SPA-GMN-DRCD-G
           IF      SPA-GMN-DRCD        =   SPACE
               MOVE    SPACE               TO  SPA-GMN-DRCD-G
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO  41013-DRNAME-SYORI-EXT
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-DRCD        TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "6005"              TO  SPA-ERRCD
               MOVE    9                   TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  SPA-GMN-DRCD
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DRCD-MAX
               IF      SPA-GMN-DRCD    =   SPA-GMN-DRCDL (IDX)
                   MOVE    SPA-GMN-DRCDLST (IDX)
                                           TO  SPA-GMN-DRCD-G
                   MOVE    SPA-DRCD-MAX    TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
      *        ドクター検索
               PERFORM 22041-DRCD-CHK-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "1"                 TO  SPA-DRCD(1:1)
               MOVE    SPA-GMN-DRCD        TO  SPA-DRCD(2:4)
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               MOVE    9               TO  SPA-GMN-CUR
           END-IF
           .
       41013-DRNAME-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ドクターチェック処理
      *****************************************************************
       22041-DRCD-CHK-SEC                   SECTION.
      *
      *    ドクター編集
           INITIALIZE                  ORCSDRSETAREA
           MOVE    "2"                 TO  ORCSDR-KBN
           MOVE    "1"                 TO  ORCSDR-KBN2
           MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
           MOVE    SPA-SRYKA           TO  ORCSDR-SRYKA
           MOVE    SPA-GMN-DRCD        TO  ORCSDR-IN-DRCD
           MOVE    SPA-GMN-DRCDLST-G   TO  ORCSDR-DRCD-TBL-G
           MOVE    SPA-DRCD-MAX        TO  ORCSDR-DRCD-MAX
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           IF      ORCSDR-ERRCD        =   ZERO
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
               MOVE    SPA-GMN-DRCDLST (SPA-DRCD-MAX)
                                           TO  SPA-GMN-DRCD-G
           ELSE
               MOVE    "6005"              TO  SPA-ERRCD
           END-IF
      *
           .
       22041-DRCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-KOUI-ALLHEN-SEC               SECTION.
      *
      *    患者の入力チェック
           IF      SPA-PTID            =   ZERO
               IF      KT01-INPUTCD(1)      NOT =   SPACE
                   MOVE    00              TO  SPA-GMN-CUR
                   MOVE    "1005"          TO  SPA-ERRCD
                   GO      TO      41011-KOUI-ALLHEN-EXT
                END-IF
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
      *    画面→ＳＰＡ編集処理
           PERFORM 410110-INPUT-GMNSET-SEC
      *
           IF      FLG-INPUT-NASI      =   1
      *        空白行をなくす
               PERFORM 420-SYOKYO-SEC
      *        カーソル位置
               PERFORM 410119-SAIINPUT-SEC
           ELSE
      *        入力コード・名称チェック処理
               MOVE    ZERO                TO  FLG-FUKU
               PERFORM 410112-KOUI-SPACHK-SEC
           END-IF
      *
           .
       41011-KOUI-ALLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面→ＳＰＡ編集処理
      *****************************************************************
       410110-INPUT-GMNSET-SEC               SECTION.
      *
      *    入力コード入力時、カーソルの保存
           IF     (MCP-EVENT           =   "ACTIVATE" ) AND
                  (WRK-WIDGET          =   "INPUTCD"  )
               MOVE    WRK-IDX2            TO  WRK-IN-MAP-IDX
           ELSE
               MOVE    ZERO                TO  WRK-IN-MAP-IDX
           END-IF
      *
      *    前回のＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  MAE-SCR-REC
      *
           MOVE    1                   TO  FLG-INPUT-NASI
      *    カーソル位置のクリア等
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      SPA-COM-CUR (IDX)   =   "1"  OR  "2"
                   IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                       MOVE    ZERO                TO  FLG-INPUT-NASI
                   END-IF
               END-IF
      *        数量チェック
               IF      SPA-COM-SURCHK (IDX)    =   "2"
                   MOVE    ZERO                TO  FLG-INPUT-NASI
               END-IF
               MOVE    SPACE               TO  SPA-COM-CUR (IDX)
               MOVE    SPACE               TO  SPA-COM-IN  (IDX)
               MOVE    SPACE               TO  SPA-COM-IN2 (IDX)
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-STR
           MOVE    ZERO                TO  FLG-KUHAKU
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     WRK-IDX2    FROM    1   BY  1
                   UNTIL       WRK-IDX2    >   MAX-GMN
      *        ＳＰＡ内の行決定
               PERFORM 41011-GYOU-SET-SEC
               IF     (KT01-INPUTCD (WRK-IDX2)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (KT01-MEISYO  (WRK-IDX2)   NOT =
                                       SPA-GMN-MEISYO-G (SPA-GMN-IDX))
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                           (SPA-GMN-IDX)
                   MOVE    SPACE               TO  SPA-COM-KZMFLG
                                                           (SPA-GMN-IDX)
                   MOVE    ZERO                TO  FLG-INPUT-NASI
               END-IF
      *
               IF     (SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                           NOT =   SPA-MAE-INPUTCD
                                                         (SPA-GMN-IDX))
                   MOVE    ZERO                TO  FLG-INPUT-NASI
               END-IF
      *
               MOVE    KT01-INPUTCD (WRK-IDX2)  TO  SPA-GMN-INPUTCD
                                                           (SPA-GMN-IDX)
               MOVE    KT01-MEISYO  (WRK-IDX2)  TO  SPA-GMN-MEISYO-G
                                                           (SPA-GMN-IDX)
      *        入力行の保存
               IF     (WRK-IN-MAP-IDX  NOT =   ZERO )  AND
                      (WRK-IDX2            =   WRK-IN-MAP-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN
                                                          (SPA-GMN-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                           (SPA-GMN-IDX)
      *            診療種別判定
                   IF     (SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                           =   SPA-MAE-INPUTCD
                                                         (SPA-GMN-IDX))
                     AND  (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)
                                           =   "."          )
                        MOVE    SPACE               TO  SPA-COM-IN2
                                                          (SPA-GMN-IDX)
                   END-IF
                   IF     (KT01-INPUTCD (WRK-IDX2)  =   SPACE )  AND
                          (SPA-MAE-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-GMN-IDX             >   SPA-GMN-MAX)
                       MOVE    1                   TO  FLG-KUHAKU
                   END-IF
               END-IF
      *
               IF      WRK-STR             =   ZERO
                   MOVE    SPA-GMN-IDX         TO  WRK-STR
               END-IF
           END-PERFORM
      *
           IF     (SPA-GMN-IDX     >=  SPA-MAX-LINE )  AND
                  (WRK-IDX2        <=  MAX-GMN  )
               PERFORM VARYING     WRK-IDX3    FROM    WRK-IDX2 BY  1
                       UNTIL      (WRK-IDX3    >   MAX-GMN   )   OR
                                  (SPA-ERRCD   NOT =   SPACE )
                   IF      KT01-INPUTCD (WRK-IDX3)  NOT =  SPACE
                       MOVE    "7004"          TO  SPA-ERRCD
                   END-IF
               END-PERFORM
           END-IF
           .
       410110-INPUT-GMNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-GYOU-SET-SEC               SECTION.
      *    行決定
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX
                   MOVE    SPA-MAX-LINE            TO  IDX
               END-IF
           END-PERFORM
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX         =   ZERO
               COMPUTE SPA-GMN-IDX     =   (SPA-GMN-PAGE   -   1)
                                       *   MAX-GMN
                                       +   WRK-IDX2
           END-IF
      *
           .
       41011-GYOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡチェック処理
      *****************************************************************
       410112-KOUI-SPACHK-SEC               SECTION.
      *
           IF      WRK-STR             =   ZERO
               MOVE    1               TO  WRK-STR
           END-IF
      *
      *    変換・削除処理
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    ZERO                TO  FLG-INS
           PERFORM VARYING SPA-GMN-IDX     FROM    WRK-STR  BY  1
                   UNTIL   SPA-GMN-IDX     >   SPA-MAX-LINE
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX)   =   "1"  )
                   PERFORM 41011-KOUI-HENDEL-SEC
               END-IF
           END-PERFORM
           IF      FLG-INS             =   1
      *        空白行をなくす
               PERFORM 420-SYOKYO-SEC
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    WRK-ERRCD           TO  SPA-ERRCD
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    入力がない時、以下をしない
           IF      FLG-INPUT-NASI      =   1
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *!!!
      *    回数行の複写
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1        >   SPA-MAX-LINE
               IF     (SPA-GMN-INPUTCD (IDX1)(1:1)  =   "*" )
                  AND (SPA-GMN-INPUTCD (IDX1)(2:1)  =   "C"
                                                   OR  "c" )
                  AND (SPA-GMN-INPUTCD (IDX1)(3:1)  =   "A"
                                                   OR  "a" )
                   PERFORM 51001-KAISU-INS-SEC
               END-IF
           END-PERFORM
      *    回数行の複写
           MOVE    ZERO                TO  IDX-KAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-MAX-LINE
               IF      SPA-GMN-INPUTCD (IDX)(1:1)  =   "*"
                   PERFORM 51000-ALLKAISU-HEN-SEC
               END-IF
           END-PERFORM
      *!!!
      *
      *    入力のチェック
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  FLG-INS
           MOVE    ZERO                TO  FLG-FUKU
           MOVE    SPACE               TO  WRK-ERRCD2
           PERFORM
                   UNTIL      (SPA-GMN-IDX     >   SPA-MAX-LINE )  OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (FLG-END     NOT =   ZERO  )
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX)   =   "1"  )
                   PERFORM 41011-KOUI-HEN2-SEC
               END-IF
      *        フリーコメントの名称の全角チェックはすべて行う
               IF     (SPA-ERRCD           =   SPACE )
                   IF    ((SPA-COM-INPUTCD(SPA-GMN-IDX)
                                           =   "810000001")  OR
                          (SPA-COM-INPUTCD(SPA-GMN-IDX)
                                           =   "008500000" ) OR
                          (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                           =   "83" OR "84") OR
                          (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                           =   "0083" 
                                           OR  "0084"
                                           OR  "0085"
                                           OR  "0086"    ))  AND
                          (SPA-GMN-MEISYO-G(SPA-GMN-IDX) 
                                       NOT =   SPACE  )
                       PERFORM 401131-MEISYO-HENKAN-SEC
                   END-IF
               END-IF
               IF     (SPA-ERRCD       =   SPACE )  AND
                      (FLG-END         =   ZERO  )
                   ADD     1                   TO  SPA-GMN-IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   "0021"
      *        診療コード入力
               PERFORM 410119-SAIINPUT-SEC
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    数量ゼロエラー
           IF      WRK-ERRCD2      NOT =   SPACE
               MOVE    WRK-ERRCD2          TO  SPA-ERRCD
           END-IF
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               IF      FLG-END         =   1
      *            ＳＰＡの内容を画面へ編集する。カーソル設定なし
                   PERFORM 500-GMNHEN-SEC
               END-IF
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *    行挿入、警告あり
           IF     (FLG-INS             =   1     )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *    点数計算・剤別チェック処理
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
      *
           IF     (SPA-KIDCD       =   SPACE  )  AND
                  (SPA-ERRCD       =   SPACE
                                   OR  "K839"
                                   OR  "0021" )  AND
                  (FLG-END         =   ZERO   )
      *        診療コード入力
               PERFORM 410119-SAIINPUT-SEC
           END-IF
      *
      *    他画面遷移時
           IF      FLG-HENKOU          =   1
      *H24.10
      *        警告はクリア
               IF      SPA-ERRCD (1:1)     =   "K"
                   PERFORM 410129-KEI-INIT-SEC
               END-IF
               MOVE    SPACE           TO  SPA-KIDCD
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    ZERO            TO  FLG-END
           END-IF
      *
           .
       410112-KOUI-SPACHK-EXT.
           EXIT.
      *H24.10
      *
      *****************************************************************
      *    画面変換・削除処理
      *****************************************************************
       410129-KEI-INIT-SEC              SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
      *
               IF      SPA-COM-CUR (IDX)   =   "1"   OR  "2"
      *        警告フラグクリア
                   EVALUATE    SPA-ERRCD
                   WHEN    "K236"
                   WHEN    "K111"
                   WHEN    "K112"
                       MOVE    SPACE           TO  SPA-COM-KZMFLG2(IDX)
                   WHEN    "K839"
                       MOVE    SPACE           TO  SPA-COM-KZMFLG3(IDX)
                   WHEN    "K200"
                       MOVE    SPACE           TO  SPA-COM-KZMFLG4(IDX)
                   WHEN    "K201"
                       MOVE    SPACE           TO  SPA-COM-KZMFLG5(IDX)
                   WHEN    "K122" 
                       MOVE    SPACE           TO  SPA-COM-KZMFLG6(IDX)
                   WHEN    "K109" 
                       MOVE    SPACE           TO  SPA-COM-KZMFLG7(IDX)
                   WHEN    "K227"
                       MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG1
                                                                  (IDX)
                   WHEN    "K228"
                       MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG
                                                                  (IDX)
                   WHEN    "K210"
                   WHEN    "K211"
                   WHEN    "K212"
                   WHEN    "K213"
                       MOVE    SPACE           TO  SPA-COM-KINKIFLG
                                                                  (IDX)
                   WHEN    OTHER
                      MOVE    SPACE            TO  SPA-COM-KZMFLG (IDX)
                   END-EVALUATE
               END-IF
           END-PERFORM
           .
       410129-KEI-INIT-EXT.
           EXIT.
      *H24.10
      *
      *****************************************************************
      *    すべての剤回数追加処理
      *****************************************************************
       51000-ALLKAISU-HEN-SEC               SECTION.
      *
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  FLG-COPY
           PERFORM VARYING     IDX3    FROM    2   BY  1
                   UNTIL      (IDX3        >   54  )
                         OR   (FLG-COPY    =   1   )
               IF      SPA-GMN-INPUTCD (IDX)(IDX3:1)
                                          =   "C"  OR  "c"
                   MOVE    1              TO  FLG-COPY
               END-IF
               IF      SPA-GMN-INPUTCD (IDX)(IDX3:1)
                                          =   "/"
                   MOVE    IDX3           TO  IDX2
               END-IF
           END-PERFORM
      *
           IF      FLG-COPY            =   1
               IF      IDX-KAI         >   ZERO
                   IF      IDX2            =   ZERO
      *            行複写
                       MOVE    SPA-GMN-INPUTCD (IDX-KAI)
                                               TO   SPA-GMN-INPUTCD(IDX)
                   ELSE
      *            日付のみ複写
                       MOVE    SPA-GMN-INPUTCD (IDX-KAI)(IDX-KAI2:)
                                               TO   SPA-GMN-INPUTCD(IDX)
                                                         (IDX2:)
                   END-IF
               END-IF
           ELSE
               IF      IDX2            >   ZERO
                   MOVE    IDX             TO  IDX-KAI
                   MOVE    IDX2            TO  IDX-KAI2
               END-IF
           END-IF
           .
       51000-ALLKAISU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    すべての剤回数追加処理
      *****************************************************************
       51001-KAISU-INS-SEC               SECTION.
      *
           MOVE    "*KIJUN"            TO  SPA-MAE-INPUTCD (IDX1)
      *
           IF      SPA-GMN-INPUTCD (IDX1 - 1)(1:1)
                                       =   "*"
               MOVE    SPACE               TO  SPA-GMN-INPUTCD (IDX1)
           ELSE
               MOVE    SPA-COM-KAISU  (IDX1 - 1)   TO  WRK-KAISU-Z
               PERFORM 510011-KAISU-HEN-SEC
               MOVE    WRK-INPUTCD     TO  SPA-GMN-INPUTCD (IDX1)
           END-IF
      *
           MOVE    ZERO                TO  FLG-COPYOK
           PERFORM VARYING     IDX3    FROM    1   BY  1
                   UNTIL       IDX3    >   SPA-MAX-LINE
               IF     (SPA-GMN-INPUTCD (IDX3)(1:1) =   "*" )
                   MOVE    1               TO  FLG-COPYOK
               END-IF
               IF     (SPA-COM-ZAIEND  (IDX3)      =   "1" )
               AND    (SPA-GMN-INPUTCD (IDX3)(1:1)  NOT =   "*" )
                 IF   (SPA-GMN-INPUTCD (IDX3 + 1)(1:1)  =   "*" )
                 OR   (FLG-COPYOK          =   ZERO  )
                   CONTINUE
                 ELSE
                   IF     (IDX3            <   SPA-MAX-LINE )
                       MOVE    SPA-COM-KAISU (IDX3)    TO  WRK-KAISU-Z
                       PERFORM 510011-KAISU-HEN-SEC
                       COMPUTE SPA-GMN-IDX     =   IDX3  +  1
                       PERFORM 41014-GYOINSN-SEC
                       IF      SPA-ERRCD       =   SPACE
                           MOVE    WRK-INPUTCD     TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)
                           ADD     1           TO  IDX3
                        END-IF
                    END-IF
                 END-IF
               END-IF
      *        終了行判定
               IF      SPA-MAE-INPUTCD (IDX3 + 1)  =   "*KIJUN"
                   MOVE    SPACE               TO  SPA-MAE-INPUTCD
                                                             (IDX3 + 1)
                   MOVE    SPA-MAX-LINE        TO  IDX3
               END-IF
           END-PERFORM
           .
       51001-KAISU-INS-EXT.
           EXIT.
      *****************************************************************
      *    回数複写処理
      *****************************************************************
       510011-KAISU-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-INPUTCD
           MOVE    SPACE               TO  WRK-KAISU-H
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   4
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
           IF      WRK-KAISU-H         =   SPACE
                                       OR  "1"
               MOVE    "*/C"                TO  WRK-INPUTCD
           ELSE
               STRING  "*"                 DELIMITED  BY  SIZE
                       WRK-KAISU-H         DELIMITED  BY  SPACE
                       "/C"                DELIMITED  BY  SIZE
                                           INTO    WRK-INPUTCD
               END-STRING
           END-IF
           .
       510011-KAISU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面変換・削除処理
      *****************************************************************
       41011-KOUI-HENDEL-SEC               SECTION.
      *
           INITIALIZE                      WRK-MOJI-AREA
      *
           INITIALIZE                  ORCSC97AREA
           MOVE    "KT01"              TO  LNK-SC97-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
           MOVE    "1"                 TO  LNK-SC97-KBN
           MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC97-INPUTCD
           CALL    "ORCSC97"           USING
                                       SPA-AREA
                                       ORCSC97AREA
                                       SPA-SCR-REC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    LNK-SC97-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    ZERO            TO  SPA-GMN-INCNT  (SPA-GMN-IDX)
               MOVE    SPACE           TO  SPA-COM-GMNFLG (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR    (SPA-GMN-IDX)
               IF      WRK-ERRCD           =   SPACE
                   MOVE    SPA-ERRCD           TO  WRK-ERRCD
               END-IF
           END-IF
      *
           MOVE    LNK-SC97-INCD       TO  WRK-CD
      *
           EVALUATE    LNK-SC97-INPUTCD(1:1)
           WHEN    SPACE
      *        削除時
      *        セットが削除の時セット内容を削除する
               IF     (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S") OR
                     ((SPA-GMN-IDX             <   SPA-MAX-LINE )  AND
                      (SPA-COM-SET  (SPA-GMN-IDX + 1)      =   "1"))
                   MOVE    SPA-GMN-IDX         TO  IDX
                   MOVE    SPACE       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
                   PERFORM 410151-SETDEL2-SEC
               ELSE
                   MOVE    SPACE       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               END-IF
               MOVE    1               TO  FLG-INS
      *
      *    剤削除時
           WHEN    "-"
               MOVE    LNK-SC97-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 41015-ZAIDEL-SEC
               MOVE    1               TO  FLG-INS
           END-EVALUATE
      *
           .
       41011-KOUI-HENDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-KOUI-HEN2-SEC               SECTION.
      *
           INITIALIZE                      WRK-MOJI-AREA
      *
           INITIALIZE                  ORCSC97AREA
           MOVE    "KT01"              TO  LNK-SC97-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-SC97-JIKANTOKU
           MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC97-INPUTCD
           CALL    "ORCSC97"           USING
                                       SPA-AREA
                                       ORCSC97AREA
                                       SPA-SCR-REC
      *
           MOVE    LNK-SC97-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
           MOVE    LNK-SC97-NYURYOKU   TO  FLG-NYURYOKU
           MOVE    LNK-SC97-INCD       TO  WRK-CD
           MOVE    LNK-SC97-INCDCNT    TO  WRK-CD-MCNT
      *    '_'を削除した時、クリアする
           IF      LNK-SC97-JODOU      >   ZERO
               MOVE    "3"             TO  SPA-COM-IN  (SPA-GMN-IDX)
           END-IF
      *    行挿入
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "+"
               MOVE    SPACE           TO  WRK-HEN-INPUTCD
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:)
                                       TO  WRK-HEN-INPUTCD
               MOVE    WRK-HEN-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 41014-GYOINSN-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "3"             TO  SPA-COM-CUR (SPA-GMN-IDX)
                   ADD     1               TO  SPA-GMN-IDX
                   MOVE    1               TO  FLG-INS
               END-IF
               GO  TO                  41011-KOUI-HEN2-EXT
           END-IF
      *    一覧へ
           IF      LNK-SC97-INPUTCD(1:2)
                                       =  "//"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               INITIALIZE                      SPA-KT98-AREA
               MOVE    LNK-SC97-INPUTCD    TO  SPA-KT98-INPUTCD
               PERFORM 410-KT98-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *
      *H28.5
      *    院外投薬名称変換
           IF     (LNK-SC97-INPUTCD(1:2)
                                       =   "/G"
                                       OR  "/g" )
            AND   (LNK-SC97-INPUTCD(3:)    =   SPACE )
            AND   (SPA-SRYYMD          >=  "20160401")
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               IF      SPA-GENERICFLG      =   "1"
                   MOVE    "0"             TO  SPA-GENERICFLG
               ELSE
                   MOVE    "1"             TO  SPA-GENERICFLG
               END-IF
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *
      *    Ｐ入力なし
           IF      LNK-SC97-INPUTCD(1:1)
                                       =  "/"
               MOVE    "6015"              TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *
           IF     LNK-SC97-NYURYOKU    =   1
      *///     セットコードに変更がない時
               IF     (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1) =   "S" ) AND
                      (SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                           =   SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX))
                   MOVE    1                   TO  FLG-SET-HEN
               ELSE
                   MOVE    ZERO                TO  FLG-SET-HEN
      *            コードが同一の時
                   IF    (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:6)
                                               =   SPA-GMN-INPUTCD
                                                   (SPA-GMN-IDX)(1:6))
                       MOVE    2               TO  FLG-SET-HEN
                   END-IF
               END-IF
      *        セットが変更削除の時セット内容を削除する
               IF     (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S" ) OR
                      ((SPA-GMN-IDX            <   SPA-MAX-LINE )  AND
                       (SPA-COM-SET  (SPA-GMN-IDX + 1)     =   "1"))
                   PERFORM 41016-SETDEL-SEC
               END-IF
               IF      FLG-SET-HEN         =   ZERO
                                           OR  2
      *        セットコードが同じ時、禁忌チェック済みを保存する
               IF      FLG-SET-HEN         =   2
                   MOVE    SPA-COM-KINKIFLG (SPA-GMN-IDX)
                                               TO  WRK-KINKIFLG
                   MOVE    SPA-COM-PTKINKI-ARI (SPA-GMN-IDX)
                                               TO  WRK-PTKINKI-ARI
                   MOVE    SPA-COM-TUKIJGNFLG  (SPA-GMN-IDX)
                                               TO  WRK-TUKIJGNFLG
                   MOVE    SPA-COM-TUKIJGNFLG1 (SPA-GMN-IDX)
                                               TO  WRK-TUKIJGNFLG1
               ELSE
                   MOVE    SPACE               TO  WRK-KINKIFLG
                   MOVE    ZERO                TO  WRK-PTKINKI-ARI
                   MOVE    SPACE               TO  WRK-TUKIJGNFLG
                   MOVE    SPACE               TO  WRK-TUKIJGNFLG1
               END-IF
               MOVE    SPA-COM-TIMEFLG(SPA-GMN-IDX)    TO  WRK-TIMEFLG
               MOVE    SPA-COM-IN     (SPA-GMN-IDX)    TO  WRK-FLG-IN
               MOVE    SPA-GMN-MEISYO-G(SPA-GMN-IDX)   TO  WRK-MEISYO-G
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    WRK-TIMEFLG     TO  SPA-COM-TIMEFLG(SPA-GMN-IDX)
               MOVE    WRK-FLG-IN      TO  SPA-COM-IN  (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
               MOVE    WRK-KINKIFLG    TO  SPA-COM-KINKIFLG
                                                        (SPA-GMN-IDX)
               MOVE    WRK-PTKINKI-ARI TO  SPA-COM-PTKINKI-ARI
                                                        (SPA-GMN-IDX)
               MOVE    WRK-TUKIJGNFLG  TO  SPA-COM-TUKIJGNFLG
                                                        (SPA-GMN-IDX)
               MOVE    WRK-TUKIJGNFLG1 TO  SPA-COM-TUKIJGNFLG1
                                                        (SPA-GMN-IDX)
      *
               END-IF
           ELSE
               MOVE    SPA-GMN-MEISYO-G(SPA-GMN-IDX)   TO  WRK-MEISYO-G
           END-IF
      *
           MOVE    LNK-SC97-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
      *
      *    診療行為一覧へ
           IF      LNK-SC97-KENSAKU    =   "1"
               INITIALIZE                      SPA-KT98-AREA
               MOVE    LNK-SC97-INCD       TO  SPA-KT98-INPUTCD
               MOVE    "1"                 TO  SPA-KT98-TYPE
               MOVE    LNK-SC97-CDSYU      TO  SPA-KT98-CDSYUBETU
               MOVE    "3"                 TO  SPA-COM-CUR (SPA-GMN-IDX)
      *
      *        診療行為一覧へ
               PERFORM 410-KT98-SYORI-SEC
               GO      TO                 41011-KOUI-HEN2-EXT
           END-IF
      *    診療種別入力
           IF      (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   ".") AND
                   (SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)  NUMERIC)
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    LNK-SC97-INPUTCD(1:4)   TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX)
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                               TO  WRK-SRYSYUKBN
      *        診療種別区分名称チェック
               PERFORM 510-SRYKBN-MEI-SEC
               MOVE    WRK-SRYSYUKBNMEI    TO  SPA-GMN-MEISYO
                                                    (SPA-GMN-IDX)
               MOVE    "*"                 TO  SPA-GMN-MEIH
                                                    (SPA-GMN-IDX)
               MOVE    WRK-SRYKBN          TO  SPA-GMN-SRYKBN
                                                    (SPA-GMN-IDX)
           ELSE
      *        文字変換・編集処理
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 410112-INPUTCD-HENKAN-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      FLG-HENKOU          =   1
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
           END-IF
      *
      *    数量ゼロエラーはエラー表示しない
           IF      SPA-ERRCD           =   "A001"
               MOVE    SPA-ERRCD           TO  WRK-ERRCD2
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *
           .
       41011-KOUI-HEN2-EXT.
           EXIT.
      *****************************************************************
      *    診療行為一覧へ
      *****************************************************************
       410-KT98-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KOUI
      *
           INITIALIZE                  SPA-KT98GMN-AREA
           IF      SPA-KT98-INPUTCD(1:2)    =   "//"
               MOVE    SPA-KT98-INPUTCD     TO  SPA-KT98-GMN-INPUT
               EVALUATE    SPA-KT98-INPUTCD(3:1)
      *        コメント一覧
                   WHEN    "C"
                   WHEN    "c"
                       MOVE    "U"             TO  SPA-KT98-GMN-SYORI
                       MOVE    "U4"            TO  SPA-KT98-GMN-SYORI3
                       MOVE    SPACE           TO  SPA-KT98-INPUTCD
      *        用法
                   WHEN    "Y"
                   WHEN    "y"
                       MOVE    "U"             TO  SPA-KT98-GMN-SYORI
                       MOVE    "U1"            TO  SPA-KT98-GMN-SYORI3
                       MOVE    SPACE           TO  SPA-KT98-INPUTCD
      *        診療種別一覧
                   WHEN    "."
                       MOVE    "."             TO  SPA-KT98-INPUTCD
      *            区分番号別
                   WHEN    "*"
                       MOVE    SPA-KT98-GMN-INPUT(3:)
                                               TO  SPA-KT98-INPUTCD
                   WHEN    "/"
                       MOVE    SPA-KT98-GMN-INPUT(3:)
                                               TO  SPA-KT98-INPUTCD
      *
      *H30.9
      *            選択式コメント一覧
                   WHEN    "S"
                   WHEN    "s"
                       IF      SPA-SRYYMD      <   "20180401"
                           MOVE    "1107"      TO  SPA-ERRCD
                           MOVE    SPACE       TO  SPA-KT98-GMN-INPUT
                       ELSE
                           PERFORM 41019-KT98-RECEKISAI-SEC
                       END-IF
                       IF      SPA-ERRCD       NOT =   SPACE
                           GO      TO  410-KT98-SYORI-EXT
                       END-IF
      *
                   WHEN    OTHER
                       MOVE    SPACE           TO  SPA-KT98-INPUTCD
               END-EVALUATE
      *        画面編集
               PERFORM 4101-KT98-SET-SEC
           ELSE
               INITIALIZE                  SPA-KT98GMN-AREA
      *        内服薬剤
               MOVE    "3"                 TO  SPA-KT98-GMN-SYORI
               IF     (SPA-GMN-IDX         =   1  )  OR 
                      (SPA-COM-KAIFLG (SPA-GMN-IDX - 1)    =   "1" ) OR
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1)   =   "S")
      *            内服薬剤
                   MOVE    "3"                 TO  SPA-KT98-GMN-SYORI
                   MOVE    1                   TO  FLG-KOUI
               ELSE
                   EVALUATE    SPA-COM-SRYKBN(SPA-GMN-IDX - 1)
                       WHEN    SPACE
      *                    診療区分が決定してない時
                           PERFORM  4101-KT98-SYRKBN-SEC
                       WHEN    "31"
                       WHEN    "32"
                       WHEN    "33"
      *                    注射
                           MOVE    "5"         TO  SPA-KT98-GMN-SYORI
                       WHEN    "23"
      *                    外薬
                           MOVE    "4"         TO  SPA-KT98-GMN-SYORI
                       WHEN    "21"
                       WHEN    "22"
      *                    内服
                           MOVE    "3"         TO  SPA-KT98-GMN-SYORI
                       WHEN    OTHER
      *                    診療行為
      *D                   MOVE    "8"         TO  SPA-KT98-GMN-SYORI
                           MOVE    "3"         TO  SPA-KT98-GMN-SYORI
                           MOVE    1           TO  FLG-KOUI
                   END-EVALUATE
               END-IF
      *        画面編集
               PERFORM 4101-KT98-SET-SEC
      *
      *        点数マスタ編集で、対象データなしの時、診療行為で検索する
               IF     (SPA-KT98-GMN-MAX     =   ZERO )  AND
                      (SPA-KT98-CDSYUBETU   =   "J"  )  AND
                      (FLG-KOUI            =   1    )
                   INITIALIZE                  SPA-KT98GMN-AREA
                   MOVE    "8"                 TO  SPA-KT98-GMN-SYORI
      *            画面編集
                   PERFORM 4101-KT98-SET-SEC
               END-IF
           END-IF
      *
      *    エラークリア
           MOVE    SPACE               TO  SPA-ERRCD
      *
      *    画面カーソルセット処理
           IF      SPA-KT98-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM01"          TO  MCP-WIDGET
           END-IF
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *    診療行為一覧へ
           MOVE    "KT98"               TO  SPA-SAKIPG
           MOVE    "KT01"               TO  SPA-MOTOPG
           MOVE    "KT01"               TO  SPA-KT98-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KT98"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       410-KT98-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療区分が決定してない時診療種別処理
      *****************************************************************
       4101-KT98-SYRKBN-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-IDX
               EVALUATE    SPA-GMN-INPUTCD(IDX)(1:3)
                   WHEN    ".21"
                       MOVE    "3"             TO  SPA-KT98-GMN-SYORI
                   WHEN    ".22"
                       MOVE    "3"             TO  SPA-KT98-GMN-SYORI
                   WHEN    ".23"
                       MOVE    "4"             TO  SPA-KT98-GMN-SYORI
                   WHEN    ".31"
                   WHEN    ".32"
                   WHEN    ".33"
                       MOVE    "5"             TO  SPA-KT98-GMN-SYORI
               END-EVALUATE
           END-PERFORM
      *
           .
      *
       4101-KT98-SYRKBN-EXT.
           EXIT.
      *
      *H30.9
      *****************************************************************
      *    選択式コメント一覧編集処理
      *****************************************************************
       41019-KT98-RECEKISAI-SEC       SECTION.
      *
      *
           IF       SPA-KT98-INPUTCD(4:1)   =   "S"
                                           OR  "s"
               MOVE    "R"             TO  SPA-KT98-GMN-SYORI
               MOVE    SPACE           TO  SPA-KT98-INPUTCD
               MOVE    SPACE           TO  SPA-KT98-GMN-SYORI2
               MOVE    "2"             TO  SPA-KT98-TYPE
           ELSE
               MOVE    "C"             TO  SPA-KT98-GMN-SYORI
               MOVE    SPACE           TO  SPA-KT98-INPUTCD
               IF     (SPA-SELCOM-IDX  =   ZERO )
                 AND  (SPA-GMN-IDX     >   1    )
                   PERFORM 510112-SELCOM-CHK-SEC
               END-IF
               IF     (SPA-SELCOM-IDX  =   ZERO  )
                   MOVE    SPACE           TO  SPA-KT98-GMN-SYORI
                   MOVE    SPACE           TO  SPA-KT98-INPUTCD
                   MOVE    "1106"          TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-COM-INPUTCD (SPA-SELCOM-IDX)
                                               TO  SPA-KT98-SELCOM-SRYCD
               END-IF
           END-IF
           .
       41019-KT98-RECEKISAI-EXT.
           EXIT.
      *****************************************************************
      *    Ｋ９８画面編集処理
      *****************************************************************
       4101-KT98-SET-SEC       SECTION.
      *
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-KT01            TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGKT98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-KT01
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
           .
      *
       4101-KT98-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード入力処理
      *****************************************************************
       410119-SAIINPUT-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-IN-IDX
           MOVE    ZERO                TO  WRK-IN-IDX2
           MOVE    ZERO                TO  WRK-MAX-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      SPA-COM-IN  (IDX)   =   "1"
                   MOVE    IDX                 TO  WRK-IN-IDX
               END-IF
               IF      SPA-COM-IN  (IDX)   =   "3"
                   MOVE    IDX                 TO  WRK-IN-IDX2
               END-IF
               IF     (SPA-COM-SET (IDX)   =   SPACE )  AND
                      (SPA-GMN-INPUTCD(IDX)    NOT =   SPACE )
                   MOVE    IDX             TO  WRK-MAX-IDX
               END-IF
      *        '_'をなくす
               INSPECT SPA-GMN-INPUTCD (IDX)
                       REPLACING   ALL "_"  BY  " "
      *        ＊をなくす
               PERFORM 51011-KAISU-CHK-SEC
      *        薬評・器評の数量なしとする
               IF     (SPA-COM-IN2     (IDX)   =   "2" )
                  AND (SPA-COM-YAKUHYO (IDX)   =   "1" )
                   IF    (SPA-COM-INPUTCD (IDX)(1:1)  =   "6"
                                                      OR  "7" )
                     OR  (SPA-COM-INPUTCD (IDX)(1:3)  =   "059" )
                       MOVE    SPACE           TO  SPA-COM-IN2 (IDX)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF     (FLG-INPUT-NASI      =   1  )  AND
                  (FLG-KUHAKU          =   1  )
               MOVE    ZERO                TO  WRK-IN-IDX
           END-IF
           IF      WRK-IN-IDX          =   ZERO
               MOVE    WRK-MAX-IDX         TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IN-IDX          TO  SPA-GMN-IDX
           END-IF
           IF      SPA-GMN-IDX         =   ZERO
               GO      TO      410119-SAIINPUT-EXT
           END-IF
      *    他画面遷移時カーソル移動なし
           IF      FLG-HENKOU          =   1
               GO      TO      410119-SAIINPUT-EXT
           END-IF
      *
      *    数量・回数入力なし
           IF     (SPA-COM-SURFLG (SPA-GMN-IDX)  =   SPACE ) AND
                  (SPA-COM-IN2    (SPA-GMN-IDX)  =   "2"   ) AND
                  (SPA-COM-KAIFLG (SPA-GMN-IDX)  =   SPACE ) AND
                  (WRK-IN-IDX2                   =   ZERO  )
               MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                            TO  WRK-INPUTCD
               MOVE    "3"                  TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
               IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)  =   "001" )
                 AND  (SPA-COM-YCOMKBN (SPA-GMN-IDX)       =   ZERO  )
      *            投薬の時のみ、回数入力へ
                   IF      SPA-COM-SRYKBN(SPA-GMN-IDX)     =   "21" OR
                                                               "22" OR
                                                               "23"
                       STRING  WRK-INPUTCD      DELIMITED  BY  SPACE
                               "*"              DELIMITED  BY  SIZE
                                                INTO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
                       END-STRING
                   ELSE
                       MOVE    SPACE            TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
                   END-IF
               ELSE
                   STRING  WRK-INPUTCD          DELIMITED  BY  SPACE
                           "_"                  DELIMITED  BY  SIZE
                                                INTO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
                   END-STRING
               END-IF
               IF      SPA-ERRCD           =   "0021"
      *            数量下限エラーはなし
                   MOVE    SPACE                TO  SPA-ERRCD
               END-IF
           END-IF
      *    名称入力へのカーソル移動
           IF      WRK-IN-IDX          >   ZERO
               IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                           =   "810000001"  ) OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                           =   "008500000"  ) OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =   "83")  OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0083"
                                           OR  "0085"
                                           OR  "0086"  )
                   IF    ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =   "83"         )  AND
                          (SPA-COM-ATAI1 (SPA-GMN-IDX)
                                       NOT =   SPACE        ))
                    OR    (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                           =   "831"        )
                       CONTINUE
                   ELSE
                       MOVE    "3"             TO  SPA-COM-IN2
                                                      (SPA-GMN-IDX)
                   END-IF
               END-IF
           END-IF
      *
      *    空白行入力時、前の行へ
           IF     (FLG-KUHAKU          =   1  )  AND
                  (SPA-GMN-IDX         >   ZERO )
               IF     (SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)
                                               NOT =   "."
                                                   AND "#" 
                                                   AND "$"  )  AND
                      (SPA-COM-KAIFLG (SPA-GMN-IDX)    =   SPACE )
      *            回数入力へ
                   MOVE    ZERO                TO  WRK-IN-IDX2
                   MOVE    ZERO                TO  FLG-ARI
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   54
                       IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(IDX:1)
                                                   =   "*"
                                                   OR  "_"
                           MOVE    1               TO  FLG-ARI
                       END-IF
                       IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(IDX:1)
                                               NOT =  SPACE
                           MOVE    IDX             TO  WRK-IN-IDX2
                       END-IF
                   END-PERFORM
                   IF     (FLG-ARI                 =   ZERO )  AND
                          (WRK-IN-IDX2             <   54   )
                       ADD     1                   TO  WRK-IN-IDX2
                       IF    ((SPA-COM-IN3 (SPA-GMN-IDX)  =   "1" ) AND
                              (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                                      NOT =   "001"))
                         OR  ((SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)
                                                       =   "S" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "84" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                                   =   "0084" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      **                 用量コメント
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:7)
                                                   =   "0992000" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      **                 用法コメント
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                                   =   "001"     ) AND
                              (SPA-COM-YCOMKBN(SPA-GMN-IDX)
                                                       =   1     ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      *H30.4             分割調剤など
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:7)
                                                   =   "0992081" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      *R02.4
      *        新コメント
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "85"   ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                                   =   "831"  ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      *
                           MOVE    "_"             TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)
                                                     (WRK-IN-IDX2:)
                       ELSE
                           MOVE    "*"             TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)
                                                     (WRK-IN-IDX2:)
                       END-IF
                   END-IF
               END-IF
      *        空白行入力時、最終行へ
               MOVE    "3"                 TO  SPA-COM-CUR
                                                     (SPA-GMN-IDX)
           END-IF
      *
           .
       410119-SAIINPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    文字列変換・編集処理
      *****************************************************************
       410112-INPUTCD-HENKAN-SEC      SECTION.
      *
      *    変換文字列編集
           PERFORM 4101121-MOJI-SET-SEC
      *
      *    回数入力
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)
                                       =   "*"
               PERFORM 4101129-KAISUU-CHK-SEC
               GO      TO      410112-INPUTCD-HENKAN-EXT
           END-IF
      *
      *    セット入力処理へ
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)
                                           =   "S"  OR  "P"
               PERFORM 410119-INPSET-SEC
               GO      TO      410112-INPUTCD-HENKAN-EXT
           END-IF
      *
      *    入力内容チェック、編集
           IF      LNK-SC97-NYURYOKU   =   1
               MOVE    LNK-SC97-INCD   TO  SPA-COM-INPUTCD(SPA-GMN-IDX)
           END-IF
           PERFORM 4101113-INPUTCD-CHK-SEC
      *
      *    名称チェック
           PERFORM 40113-MEISYOU-CHK-SEC
      *
           .
       410112-INPUTCD-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    入力内容チェック、編集処理
      *****************************************************************
       4101113-INPUTCD-CHK-SEC       SECTION.
      *
      *    初診料でコードの後ろの日付の入っている時初診料算定済みとする
           IF     (SPA-GMN-IDX         =   1  )     AND
                  (SPA-GMN-MAX1        =   ZERO  )  AND
                  (LNK-SC97-SURYOCNT   =   7     )  AND
                  (SPA-SYOYMD          =   SPACE )
      *            包括保険は対象外
              AND (SPA-HKNCOMBINUM     NOT =   "9999"  )
      *            治験は対象外
              AND (SPA-CHIKENKBN           =   SPACE   )
               PERFORM 4101125-SYOSIN-CHK-SEC
               IF     (SPA-KIDCD       NOT =   SPACE )
                   OR (SPA-ERRCD       NOT =   SPACE )
                   GO      TO      4101113-INPUTCD-CHK-EXT
               END-IF
           END-IF
      *
      *    入力コード変更ありの時
           IF      LNK-SC97-NYURYOKU   =   1
               CONTINUE
           ELSE
      *        名称を戻す
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:1)
                                           =   "1"  )
                 AND  (SPA-COM-AUTOKBN2 (SPA-GMN-IDX)
                                           =   SPACE )
                   MOVE    SPA-COM-NAME(SPA-GMN-IDX)
                                           TO  SPA-GMN-MEISYO
                                                        (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  SPA-COM-IN3 (SPA-GMN-IDX)
      *    入力値の入力判定用
           MOVE    SPA-COM-ATAI1 (SPA-GMN-IDX) TO  WRK-HZN-ATAI1
           MOVE    SPA-COM-ATAI2 (SPA-GMN-IDX) TO  WRK-HZN-ATAI2
           MOVE    SPA-COM-ATAI3 (SPA-GMN-IDX) TO  WRK-HZN-ATAI3
           MOVE    SPA-COM-ATAI4 (SPA-GMN-IDX) TO  WRK-HZN-ATAI4
           MOVE    SPA-COM-ATAI5 (SPA-GMN-IDX) TO  WRK-HZN-ATAI5
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
      *    入力コード変更ありの時
           IF      LNK-SC97-NYURYOKU   =   1
               MOVE    SPACE               TO  LNK-SC92-KBN
           ELSE
               MOVE    "5"                 TO  LNK-SC92-KBN
           END-IF
           IF      FLG-HENKOU          =   1
      *        画面遷移前
               IF     (LNK-SC97-NYURYOKU       =   1 ) OR
                      (SPA-COM-CHKFLG (SPA-GMN-IDX)  =   SPACE)
                   MOVE    "6"                 TO  LNK-SC92-KBN
               END-IF
           END-IF
      *    入力値編集あり
           MOVE    "1"                 TO  LNK-SC92-INFLG
           MOVE    LNK-SC97-INPUT-G    TO  LNK-SC92-INPUT-G
           MOVE    SPA-NAI-TIMEFLG     TO  LNK-SC92-TIMEFLG
      *    警告チェックあり
           MOVE    "1"                 TO  LNK-SC92-KEIFLG
      *
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "KT01"              TO  LNK-SC92-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
      *****MOVE    SPA-KT01-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           MOVE    LNK-SC92-INPUT-G    TO  LNK-SC97-INPUT-G
      *!!! MOVE    LNK-SC92-TIMEFLG    TO  SPA-NAI-TIMEFLG
      *
           MOVE    LNK-SC92-GMN-IDX    TO  SPA-GMN-IDX
      *    カーソル位置設定
           PERFORM 41011139-INFLG-HEN-SEC
      *
      *    画像診断のＣＴ．ＭＲＩは２回目のコードが違うので
      *    算定回数のチェックなし
           IF     (SPA-COM-TNSSRYSYUKBN (SPA-GMN-IDX)
                                               =   "723"
                                               OR  "724"  )  AND
                  (SPA-COM-HOUKATUTEIGENKBN(SPA-GMN-IDX)
                                               =   "201"
                                               OR  "202"
                                               OR  "203"  )  AND
                   (SPA-COM-KAISU (SPA-GMN-IDX)
                                               =   1      )
               MOVE    SPACE           TO  LNK-SC92-ERRCD(3)
           END-IF
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  AND
                  (LNK-SC92-ERRMSG NOT =   SPACE )
               MOVE    LNK-SC92-ERRMSG     TO  SPA-ERRMSG2
           END-IF
      *
           PERFORM VARYING     IDX-ERR FROM    1  BY  1
                   UNTIL      (IDX-ERR     >   10     )  OR
                              (SPA-ERRCD   NOT =   SPACE)
               IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                   MOVE    LNK-SC92-ERRCD(IDX-ERR)     TO  SPA-ERRCD
                   IF      IDX-ERR             =   3   OR  6
                       MOVE    LNK-SC92-ERRMSG     TO  SPA-ERRMSG2
                   END-IF
               END-IF
           END-PERFORM
      *
      *    その他値の入力の有無
           IF     (WRK-HZN-ATAI1   =   SPA-COM-ATAI1 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI2   =   SPA-COM-ATAI2 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI3   =   SPA-COM-ATAI3 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI4   =   SPA-COM-ATAI4 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI5   =   SPA-COM-ATAI5 (SPA-GMN-IDX))
               MOVE    ZERO                TO  FLG-IN-ATAI
           ELSE
               MOVE    1                   TO  FLG-IN-ATAI
           END-IF
      *
      *    小児科外来診療科の時、年齢エラーはそのまま
      *    IF     (SPA-SYONIKA-ARI3    NOT =   ZERO )   AND
      *           (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:1)  =   "1" )
      *        小児科外来診療料以外はエラー
      *      AND  (SPA-COM-SRYKBN  (SPA-GMN-IDX)  =   "13"   )
      *H28.5
      *      AND  (SPA-COM-INPUTCD (SPA-GMN-IDX)
      *                                    NOT =   "113018210")
      *        IF      SPA-ERRCD           =   "0032"
      *            MOVE    SPACE               TO  SPA-ERRCD
      *        END-IF
      *    END-IF
      *---(01.00.01) LINE ADD  START  ----
      *    警告判定
           IF      SPA-ERRCD(1:1)      =   "K"
             AND  (SPA-ERRCD       NOT =   "K033" )
               IF      SPA-COM-KZMFLG (SPA-GMN-IDX)  =   SPACE
                   MOVE    "1"             TO  SPA-COM-KZMFLG
                                                          (SPA-GMN-IDX)
                   MOVE    "2"             TO  SPA-COM-CUR(SPA-GMN-IDX)
               ELSE
                   MOVE    SPACE           TO  SPA-ERRCD
                   MOVE    SPACE           TO  SPA-COM-CUR(SPA-GMN-IDX)
               END-IF
           END-IF
      *---(01.00.01) LINE ADD  END    ----
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPACE           TO  SPA-COM-CHKFLG  (SPA-GMN-IDX)
               MOVE    SPACE           TO  SPA-COM-HCHKFLG (SPA-GMN-IDX)
               GO      TO      4101113-INPUTCD-CHK-EXT
           END-IF
      *
      *    時間外加算の時、ＳＰＡへ
      *    IF      SPA-COM-TIMEFLG(SPA-GMN-IDX)  NOT =   ZERO
      *        MOVE    SPA-COM-TIMEFLG(SPA-GMN-IDX)
      *                                    TO  SPA-NAI-TIMEFLG
      *    END-IF
      *!!  保険適用区分が保険適用外のもの（ユーザー登録の自費分）
           IF      SPA-COM-HKNTEKKBN(SPA-GMN-IDX)  =   2
      *        数量を金額とする
      *????    IF      SPA-COM-KISOTEN (SPA-GMN-IDX)  =   ZERO
               IF      SPA-COM-TEN     (SPA-GMN-IDX)  =   ZERO
                   MOVE    1               TO  SPA-COM-SURYO
                                                          (SPA-GMN-IDX)
                   MOVE    WRK-SURYO       TO  SPA-COM-KEI(SPA-GMN-IDX)
                   MOVE    WRK-SURYO       TO  SPA-COM-JIHIMONEY
                                                          (SPA-GMN-IDX)
                   MOVE    WRK-SURYO       TO  SPA-GMN-KEI(SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
               ELSE
                   MOVE    "2"             TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
           END-IF
      *
           .
       4101113-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル位置設定処理
      *****************************************************************
       41011139-INFLG-HEN-SEC       SECTION.
      *
      *    数量以降入力なし（’＿’編集用）
           IF     (WRK-SUR-MCNT        =   ZERO )  AND
                  (WRK-ACCT-MCNT       =   ZERO )  AND
                  (WRK-MCNT1           =   ZERO )  AND
                  (WRK-MCNT2           =   ZERO )  AND
                  (WRK-MCNT3           =   ZERO )  AND
                  (WRK-MCNT4           =   ZERO )  AND
                  (WRK-MCNT5           =   ZERO )  AND
                  (FLG-KAISU           =   ZERO )
               MOVE    ZERO                TO  FLG-IN1
      *        薬剤と数量入力必須の診療行為、薬剤コメント
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)   =   "059"
                   MOVE    "7"                     TO  WRK-MOJI
               ELSE
                   MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)(1:1)
                                                   TO  WRK-MOJI
               END-IF
      *        数量の入力の有無
               IF     (WRK-MOJI                    =   "6" OR
                                                       "7" )  OR
                     ( SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                                   =   "001" )
      *                  用量コメント
                 OR  ( SPA-COM-INPUTCD(SPA-GMN-IDX)(1:7)
                                                   =   "0992000" )
      *            薬剤・器材・用法
                   MOVE    1                   TO  FLG-IN1
               END-IF
               IF      WRK-MOJI                =   "1"
                   IF  (SPA-COM-SRYSYUKBN(SPA-GMN-IDX)(1:1)
                                                    NOT =   "7" ) AND
                       (SPA-COM-KZMCOMPSIKIBETU(SPA-GMN-IDX)
                                                    NOT =   0   )
                       MOVE    1                   TO  FLG-IN1
                   END-IF
               END-IF
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "84" )  OR
                      (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                                   =   "0084")
      *R02.3
               OR     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "85" )
               OR     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                                   =   "831"  )
      *            コメント
                   MOVE    1                   TO  FLG-IN1
               END-IF
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:5)
                                                   =   "09500" OR
                                                       "09600" )
      *            自費
                   IF     (SPA-COM-TEN (SPA-GMN-IDX)   =   ZERO )
                       MOVE    1                   TO  FLG-IN1
                   END-IF
               END-IF
      *        用法コメント
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                               =   "001" )
                 AND  (SPA-COM-YCOMKBN(SPA-GMN-IDX) =   1 )
                   MOVE    1                   TO  FLG-IN1
               END-IF
      *H30.4   分割調剤など
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:7)
                                               =   "0992081" )
                   MOVE    1                   TO  FLG-IN1
               END-IF
      *
               IF      FLG-IN1             =   1
                   MOVE    "2"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN3 (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           .
       41011139-INFLG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料算定済みチェック処理
      *****************************************************************
       4101125-SYOSIN-CHK-SEC       SECTION.
      *
           IF       SPA-COM-INPUTCD(SPA-GMN-IDX)
                                       =   "111700110"  OR
                                           "111700210"  OR
                                           "111000110"  OR
                                           "111003610"  OR
                                           "113003510"  OR
                                           "113003710"
      *        労災追加
                                       OR  "101110010"
      *        紹介状なし追加
                                       OR  "111012510"
      *        妥結率５割以下
                                       OR  "111012710"
               CONTINUE
           ELSE
               GO      TO      4101125-SYOSIN-CHK-EXT
           END-IF
      *
      *    数量の日付変換
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-SURYO-S1        TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           MOVE    SPACE               TO  WRK-SYOKAI-YMD
           IF      SGDAY-RC            =   ZERO
      *H25.4   紹介状なしは、Ｈ２５.4から
               IF      (SPA-COM-INPUTCD(SPA-GMN-IDX) =  "111012510")
                  AND  (SGDAY-SDAY             <   "20130401" )
                   MOVE    "1103"              TO  SPA-ERRCD
                   GO      TO      4101125-SYOSIN-CHK-EXT
               END-IF
      *
      *H27.1   妥結率５割以下　Ｈ２７.１から
               IF      (SPA-COM-INPUTCD(SPA-GMN-IDX) =  "111012710")
                  AND  (SGDAY-SDAY             <   "20150101" )
                   MOVE    "1104"              TO  SPA-ERRCD
                   GO      TO      4101125-SYOSIN-CHK-EXT
               END-IF
      *
               IF      SGDAY-SDAY(1:6)     >=  SPA-SRYYMD(1:6)
                   MOVE    "1101"              TO  SPA-ERRCD
               ELSE
                   MOVE    SGDAY-SDAY          TO  WRK-SYOKAI-YMD
               END-IF
           ELSE
               MOVE    "1102"              TO  SPA-ERRCD
           END-IF
           IF      WRK-SYOKAI-YMD  NOT =   SPACE
               IF     (SPA-LSTYMD      NOT =   SPACE      )  AND
                      (WRK-SYOKAI-YMD      >   SPA-LSTYMD )
                   MOVE    "1206"              TO  SPA-ERRCD
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   MOVE    WRK-SYOKAI-YMD      TO  SPA-SYOSIN-YMD
                   MOVE    SPA-SYOSIN-YMD      TO  SPA-SYOKAI-YMD
                   MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)
                                               TO  SPA-SYOKAI-SRYCD
                   MOVE    SPA-GMN-IDX         TO  SPA-SYOKAI-IDX
                   MOVE    "0140"              TO  SPA-KIDCD
               END-IF
           END-IF
      *
           .
       4101125-SYOSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力文字を数字変換処理
      *****************************************************************
       4101121-MOJI-SET-SEC        SECTION.
      *
           MOVE    LNK-SC97-SURYOCNT   TO  WRK-SUR-MCNT
           MOVE    LNK-SC97-KAISUCNT   TO  WRK-ACCT-MCNT
           MOVE    LNK-SC97-SURYO      TO  WRK-SURYO
           MOVE    LNK-SC97-KAISU      TO  WRK-KAISU
           MOVE    LNK-SC97-KAISUFLG   TO  FLG-KAISU
           MOVE    LNK-SC97-MCNT1      TO  WRK-MCNT1
           MOVE    LNK-SC97-MCNT2      TO  WRK-MCNT2
           MOVE    LNK-SC97-MCNT3      TO  WRK-MCNT3
           MOVE    LNK-SC97-MCNT4      TO  WRK-MCNT4
           MOVE    LNK-SC97-MCNT5      TO  WRK-MCNT5
           .
       4101121-MOJI-SET-EXT.
           EXIT.
      *****************************************************************
      *    入力項目編集処理
      *****************************************************************
       4101122-KOMOKU-HENSYU-SEC      SECTION.
      *
      *    セットの入力値
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
      *    警告チェックあり
           MOVE    "1"                 TO  LNK-SC92-KEIFLG
      *    入力値編集あり
           MOVE    "1"                 TO  LNK-SC92-INFLG
           MOVE    LNK-SC97-INPUT-G    TO  LNK-SC92-INPUT-G
           MOVE    SPA-NAI-TIMEFLG     TO  LNK-SC92-TIMEFLG
           MOVE    "KT01"               TO  LNK-SC92-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  LNK-SC92-SRYCD
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           MOVE    LNK-SC92-INPUT-G    TO  LNK-SC97-INPUT-G
      *
           .
       4101122-KOMOKU-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    回数入力チェック処理
      *****************************************************************
       4101129-KAISUU-CHK-SEC           SECTION.
      *
      *    回数に変更があった時、剤内の警告はクリアする
           IF     SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                   NOT =   SPA-MAE-INPUTCD(SPA-GMN-IDX)
               COMPUTE IDX-K           =   SPA-GMN-IDX   -   1
               PERFORM VARYING     IDX-K2  FROM    IDX-K  BY  -1
                       UNTIL       IDX-K2  <   1
                  IF      SPA-COM-ZAIEND (IDX-K2)  =   "1"
                      MOVE    1                TO  IDX-K2
                  ELSE
                      IF      SPA-COM-SRYKBN (IDX-K2)  =  "80"
                                                       OR "54"
                           MOVE    SPACE           TO  SPA-COM-KZMFLG
                                                            (IDX-K2)
                           MOVE    SPACE           TO  SPA-COM-KZMFLG2
                                                            (IDX-K2)
                           MOVE    SPACE           TO  SPA-COM-KZMFLG3
                                                            (IDX-K2)
                      END-IF
                  END-IF
               END-PERFORM
           END-IF
      *
      *    クリア
           MOVE    SPACE           TO  SPA-COM-INPUTCD(SPA-GMN-IDX)
           INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
      *
      *    回数（未入力時、１回）
           IF      WRK-ACCT-MCNT       =   ZERO
               MOVE    1               TO  SPA-COM-KAISU (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
           ELSE
               MOVE    "1"             TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
               MOVE    WRK-KAISU       TO  SPA-COM-KAISU (SPA-GMN-IDX)
               IF      SPA-COM-KAISU(SPA-GMN-IDX)
                                       =   ZERO
                   MOVE    1           TO  SPA-COM-KAISU (SPA-GMN-IDX)
               END-IF
           END-IF
           MOVE    SPA-COM-KAISU(SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
      *
      *    入院回数・日数
           INITIALIZE                  SPA-COM-NYUIN-AREA (SPA-GMN-IDX)
           PERFORM VARYING     IDD2    FROM    1   BY  1
                   UNTIL       IDD2    >   31
              IF      LNK-SC97-NYU-DAY (IDD2)   NOT =   ZERO
                   IF      SPA-K02-NYUDAY (IDD2)    =   ZERO
                       MOVE    1               TO  SPA-COM-NYUINDAY
                                                      (SPA-GMN-IDX IDD2)
                       ADD     1               TO  SPA-COM-NYU-KAI
                                                      (SPA-GMN-IDX)
                   ELSE
                       IF      SPA-ERRCD       =   SPACE
                           MOVE    IDD2             TO  SPA-ERRMSG2
                       END-IF
                       MOVE    "6001"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
      *    保険組合せ期間との日付チェック
           IF      SPA-COB-TEKSTYMD(1:6)   =   SPA-SRYYMD(1:6)
               MOVE    SPA-COB-TEKSTYMD(7:2)   TO  WRK-STDD
           ELSE
               MOVE    01                      TO  WRK-STDD
           END-IF
           IF      SPA-COB-TEKEDYMD(1:6)   =   SPA-SRYYMD(1:6)
               MOVE    SPA-COB-TEKEDYMD(7:2)   TO  WRK-EDDD
           ELSE
               MOVE    SPA-SRYLST-DD           TO  WRK-EDDD
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   31  )
                          OR  (SPA-ERRCD   NOT =   SPACE )
              IF      LNK-SC97-NYU-DAY (IDX)   NOT =   ZERO
                   IF     (IDX             >=  WRK-STDD )  AND
                          (IDX             <=  WRK-EDDD )
                       CONTINUE
                   ELSE
                       MOVE    "9038"          TO  SPA-ERRCD
                       MOVE    SPACE           TO  SPA-ERRMSG2
                       STRING  WRK-STDD
                               "日〜"
                               WRK-EDDD
                              "日が当月の有効期間です。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG2
                       END-STRING
                   END-IF
               END-IF
           END-PERFORM
      *!!
      *    登録済み日
           IF      SPA-NAI-LSTDD       >   ZERO
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   31  )
                              OR  (SPA-ERRCD   NOT =   SPACE )
                   IF      LNK-SC97-NYU-DAY (IDX)  NOT =   ZERO
                       IF     (IDX             <=  SPA-NAI-LSTDD )
                           MOVE    "6007"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       4101129-KAISUU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       410119-INPSET-SEC           SECTION.
      *
      *    時間外がないこと
           IF      LNK-SC97-JIKAN      NOT =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               GO  TO                 410119-INPSET-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-SINSATU-SAI
      *
           MOVE    SPACE               TO  WRK-ERRCD
           PERFORM 4101191-INPUTSET-SYORI-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-ERRCD       TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       410119-INPSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       4101191-INPUTSET-SYORI-SEC      SECTION.
      *
      *    約束処方の初期処理
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
               IF      FLG-TOROKU      NOT =   "2"
                   PERFORM 4101192-INPSET-SET-SEC
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4101191-INPUTSET-SYORI-EXT
           END-IF
      *
           INITIALIZE                  ORCSCSETHENAREA
           MOVE    "KT01"               TO  ORCSCSETHEN-PG
      *    セット名
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  ORCSCSETHEN-SRYCD
      *    対象行
           MOVE    SPA-GMN-IDX         TO  ORCSCSETHEN-IDX
      *    ORCSC92検索用
           MOVE    SPA-SYORIFLG        TO  ORCSCSETHEN-SYORIFLG
           MOVE    SPA-NENREI          TO  ORCSCSETHEN-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  ORCSCSETHEN-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  ORCSCSETHEN-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  ORCSCSETHEN-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  ORCSCSETHEN-LSTSRYKAMEI
      **** MOVE    SPA-KT01-TEISEI-AREA TO  ORCSCSETHEN-TEISEI-AREA
      *
           CALL    "ORCSCSETHEN"       USING
                                       ORCSCSETHENAREA
                                       SPA-AREA
                                       SPA-SCR-REC
      *    展開後の位置
           MOVE    ORCSCSETHEN-IDX     TO  SPA-GMN-IDX
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    ORCSCSETHEN-ERRMSG2     TO  SPA-ERRMSG2
           END-IF
      *
           .
       4101191-INPUTSET-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    約束処方の処理
      *****************************************************************
       4101192-INPSET-SET-SEC           SECTION.
      *
      *    入力位置のセット
           MOVE    ZERO                TO  WRK-IN-SURYO
           MOVE    SPA-GMN-IDX         TO  WRK-IN-IDX
      *    数量以降入力なし
           IF     (WRK-SUR-MCNT        =   ZERO )  AND
                  (WRK-ACCT-MCNT       =   ZERO )  AND
                  (WRK-MCNT1           =   ZERO )  AND
                  (WRK-MCNT2           =   ZERO )  AND
                  (WRK-MCNT3           =   ZERO )  AND
                  (WRK-MCNT4           =   ZERO )
               IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)  =   "S"
                   MOVE    1                   TO  WRK-IN-SURYO
                   MOVE    "1"                 TO  SPA-COM-IN
                                                         (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *    数量・回数編集
           PERFORM 4101122-KOMOKU-HENSYU-SEC
           MOVE    WRK-CD(1:WRK-CD-MCNT)   TO  WRK-INPUTCD
      *
           .
       4101192-INPSET-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為、計計算処理、編集処理
      *****************************************************************
       510-TBLHEN-SEC           SECTION.
      *
      *    初診料入力は日毎
           MOVE    ZERO                TO  SPA-SYOSIN
      *
           MOVE    SPA-AKUSEI-FLG      TO  FLG-MAE-AKUSEI
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSC100AREA
           MOVE    "1"                 TO  ORCSC100-KBN
           MOVE    FLG-TOROKU          TO  ORCSC100-TOROKU
           MOVE    FLG-INIT            TO  ORCSC100-INIT
           MOVE    FLG-KENSADEL        TO  ORCSC100-KENSADEL
           MOVE    FLG-RIHTEI          TO  ORCSC100-RIHTEI
           CALL    "ORCSCKT100"        USING
                                       ORCSC100AREA
                                       SPA-AREA
                                       SPA-KT01
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    悪性検査一覧選択後のエラー
           IF     (FLG-MAE-AKUSEI      =   1   )
             AND  (SPA-ERRCD       NOT =   SPACE)
               GO      TO      510-TBLHEN-EXT
           END-IF
      *
      *    悪性腫瘍検査一覧へ
           IF      SPA-AKUSEI-FLG      =   1
               PERFORM 5101-AKUSEI-KT98-SEC
               GO      TO      510-TBLHEN-EXT
           END-IF
      *
      *H30.9
      *    選択コメント一覧へ
           IF     (SPA-SELCOM-FLG      =   1   )
      *    中途データ展開時はなし
           AND    (FLG-INIT            =   ZERO )
               PERFORM 51011-SELCOM-KT98-SEC
               GO    TO  510-TBLHEN-EXT
           END-IF
      *TEST
      *
           .
        510-TBLHEN-EXT.
           EXIT.
      *
      *TEST
      *H30.9
      *****************************************************************
      *    選択コメント一覧へ処理
      *****************************************************************
       510112-SELCOM-CHK-SEC       SECTION.
      *
           COMPUTE IDX-STR             =   SPA-GMN-IDX  -  1
      *    直前に＊があれば、エラー
           IF     (SPA-COM-ZAIEND (IDX-STR)    =   "1" )
              AND (SPA-COM-KAIFLG (IDX-STR)    =   "1" )
               GO      TO      510112-SELCOM-CHK-EXT
           END-IF
      *
           PERFORM VARYING    IDX      FROM    IDX-STR  BY  -1
                   UNTIL     (IDX      <       1   )
                        OR   (SPA-SELCOM-IDX   >   ZERO  )
               IF     (SPA-COM-INPUTCD (IDX)(1:1)  =   "1" )
                  AND (SPA-COM-RECEKISAI(IDX)      =   "1"
                                                   OR  "2"  )
                   MOVE    IDX                 TO  SPA-SELCOM-IDX
                   MOVE    1                   TO  SPA-SELCOM-FLG
               END-IF
      *        同じ剤内での検索
               IF      (IDX            NOT =   IDX-STR )
                   AND (SPA-COM-ZAIEND (IDX)   =   "1" )
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
           .
       510112-SELCOM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択コメント一覧へ処理
      *****************************************************************
       51011-SELCOM-KT98-SEC       SECTION.
      *
           MOVE    SPA-SELCOM-IDX      TO  SPA-GMN-IDX
      *
           INITIALIZE                      SPA-KT98-AREA
           MOVE    "C"                 TO  SPA-KT98-GMN-SYORI
           MOVE    "//S"               TO  SPA-KT98-INPUTCD
           MOVE    "3"                 TO  SPA-COM-CUR (SPA-SELCOM-IDX)
      *
      *    診療行為一覧へ
           PERFORM 410-KT98-SYORI-SEC
           .
       51011-SELCOM-KT98-EXT.
           EXIT.
      *TEST
      *TEST
      *!!!!
      *
      *
      *****************************************************************
      *    最終行に回数の入力があるかのチェック処理
      *****************************************************************
       51011-KAISU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-KAISU
           PERFORM VARYING     IDX-KAI FROM    1   BY  1
                   UNTIL       IDX-KAI     >   54
               IF      FLG-OK              =   1
                   IF      SPA-GMN-INPUTCD(IDX)(IDX-KAI:1) NUMERIC
                       MOVE    1               TO  FLG-KAISU
                   END-IF
               ELSE
                   IF      SPA-GMN-INPUTCD(IDX)(IDX-KAI:1)   =   "*"
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
           END-PERFORM
      *    ＊ 以降に入力のない時、＊を削除
           IF      FLG-KAISU           =   ZERO
               INSPECT SPA-GMN-INPUTCD(IDX)
                       REPLACING   FIRST "*"  BY  " "
           END-IF
           .
       51011-KAISU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌チェック処理
      *****************************************************************
       51010-KNKCHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-KINKI
      *    相互作用チェック期間
           IF      SPA-INTERACT-KIKAN  >   ZERO
               INITIALIZE                  ORCSCKNKCHKAREA
               MOVE    "2"                 TO  ORCSCKNKCHK-KBN 
               CALL    "ORCSCKTKNKCHK"     USING
                                           ORCSCKNKCHKAREA
                                           SPA-AREA
                                           SPA-KT01
                                           SPA-SCR-REC
               IF      SPA-KT011-MAX        >   ZERO
                   MOVE    1                   TO  FLG-KINKI
                   MOVE    1                   TO  FLG-ALLFLG
                   PERFORM 51020-KT011-SYORI-SEC
               END-IF
           END-IF
           .
       51010-KNKCHK-EXT.
           EXIT.
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       510-SRYKBN-MEI-SEC            SECTION.
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
                       MOVE    "0006"          TO  SPA-ERRCD
                   WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUKBNMEI
           END-SEARCH 
      *
           .
       510-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為名入力処理
      *****************************************************************
       40113-MEISYOU-CHK-SEC        SECTION.
      *
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)   =  "810000001"
                                                   OR "008500000" 
                                                   OR "008600000" )
               MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX)
               MOVE    "1"                 TO  SPA-COM-MEIFLG
                                                       (SPA-GMN-IDX)
           END-IF
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  = "83" OR
                                                         "84")   OR
      *R02.3
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  = "85" ) OR
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  = "0083" OR
                                                         "0084" OR
                                                         "0085" OR
                                                         "0086")
               IF     (LNK-SC97-NYURYOKU   =   ZERO )  AND
                      (FLG-IN-ATAI         =   ZERO )  AND
                      (SPA-COM-ATAI1 (SPA-GMN-IDX)
                                           =   SPACE)  AND
                     ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =   "83"   )  OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0083"
                                           OR  "0085"
                                           OR  "0086" ))
                   MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX)
               END-IF
               MOVE    "1"             TO  SPA-COM-MEIFLG
                                                       (SPA-GMN-IDX)
           END-IF
           IF    ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  =  "81"
                                                       OR "83"
                                                       OR "84") OR
      *R02.3
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  =  "85" ) OR
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  =  "0083"
                                                       OR "0084"
                                                       OR "0085"
                                                       OR "0086")) AND
                  (SPA-GMN-MEISYO-G(SPA-GMN-IDX) NOT =   SPACE)
      *
      *        半角の空白を全角に置きかえる
               PERFORM 401131-MEISYO-HENKAN-SEC
           END-IF
      *
           .
       40113-MEISYOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称チェック・全角置換処理
      *****************************************************************
       401131-MEISYO-HENKAN-SEC        SECTION.
      *
      *    フリーコメントは１桁目から
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)   =   "810000001"
                                                  OR  "008500000"
                                                  OR  "008600000"
               MOVE    1                   TO  IDX-STR
      *R02.4   MOVE    80                  TO  WRK-LEN
               MOVE    100                 TO  WRK-LEN
           ELSE
               MOVE    3                   TO  IDX-STR
      *R02.3   MOVE    78                  TO  WRK-LEN
               MOVE    100                 TO  WRK-LEN
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                           =   "831" 
                   MOVE    138                 TO  WRK-LEN
               END-IF
      *
               IF     (SPA-GMN-MEISYO-G(SPA-GMN-IDX)(1:2)
                                           =   "* "
                                           OR  SPACE    )
                   CONTINUE
               ELSE
      *            開始位置より前に入力があれば元に戻してエラー
                   MOVE    "0100"          TO  SPA-ERRCD
                   MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
                   MOVE    2               TO  SPA-GMN-CUR
                   MOVE    SPACE           TO  SPA-GMN-MEISYO-G
                                                           (SPA-GMN-IDX)
                   MOVE    SPA-COM-NAME (SPA-GMN-IDX)
                                           TO  SPA-GMN-MEISYO
                                                           (SPA-GMN-IDX)
               END-IF
           END-IF
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           MOVE    SPA-GMN-MEISYO-G (SPA-GMN-IDX)(IDX-STR:)
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO-G
                                                       (SPA-GMN-IDX)
                                                            (IDX-STR:)
               MOVE    "0080"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               MOVE    2               TO  SPA-GMN-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO-G
                                                       (SPA-GMN-IDX)
                                                            (IDX-STR:)
           END-IF
      *
           .
       401131-MEISYO-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行挿入処理
      *****************************************************************
       41014-GYOINSN-SEC         SECTION.
      *
           IF      SPA-GMN-MAX         =   SPA-MAX-LINE
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF
      *
           IF      SPA-GMN-IDX         >=  SPA-MAX-LINE
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      IDX             =   SPA-GMN-IDX
                   CONTINUE
               ELSE
                   ADD     1               TO  IDY
                   MOVE    WRK-COM-TBLREC (IDY) 
                                           TO  SPA-COM-TBLREC(IDX)
                   MOVE    WRK-GMN-TBLREC (IDY) 
                                           TO  SPA-GMN-TBLREC(IDX)
               END-IF
               IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                   MOVE    IDX                 TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       41014-GYOINSN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤削除処理
      *****************************************************************
       41015-ZAIDEL-SEC              SECTION.
      *
      *****IF      (SPA-GMN-IDX        =   1   )  OR
      *            (SPA-COM-ZAIEND(SPA-GMN-IDX - 1 )
      *                                =   "1" )
      *        CONTINUE
      *    ELSE
      *        MOVE    "0023"              TO  SPA-ERRCD
      *        MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
      *        GO      TO      41015-ZAIDEL-EXT
      **** END-IF
      *
      *    ’−’は剤内のどの位置でも削除とする
      *    対象より、前
           IF      (SPA-GMN-IDX        =   1   )  OR
                   (SPA-COM-ZAIEND(SPA-GMN-IDX - 1 )
                                       =   "1" )
               CONTINUE
           ELSE
               PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  -1
                       UNTIL       IDX     <   1
                   IF      IDX             <   SPA-GMN-IDX
                       IF      SPA-COM-ZAIEND(IDX)     =   "1"
                          MOVE    1               TO  IDX
                       ELSE
                           MOVE    SPACE       TO  SPA-GMN-INPUTCD(IDX)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *    後ろ
           PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  1
                   UNTIL      (IDX     >  SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
      *        セットが削除の時セット内容を削除する
               IF     (SPA-MAE-INPUTCD(IDX)(1:1)   =   "S") OR
                     ((IDX                     <   SPA-MAX-LINE  )  AND
                      (SPA-COM-SET  (IDX + 1)  =   "1"))
                   MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
                   PERFORM 410151-SETDEL2-SEC
                   MOVE    SPA-MAX-LINE        TO  IDX
               ELSE
      *
                   MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
                   IF    ((IDX                 <   SPA-MAX-LINE )  AND
                          (SPA-GMN-SRYKBN (IDX + 1)
                                               NOT =   SPACE)) OR
                          (SPA-COM-ZAIEND(IDX)     =   "1"   )
                       MOVE    SPA-MAX-LINE            TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       41015-ZAIDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット削除処理（剤削除）
      *****************************************************************
       410151-SETDEL2-SEC              SECTION.
      *
           COMPUTE IDX2                =   IDX     +   1
      *
           PERFORM VARYING     IDX3    FROM    IDX2    BY  1
                   UNTIL      (IDX3    >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD (IDX3)  =   SPACE)
               IF      SPA-COM-SET     (IDX3)   =   "1"
                   MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX3)
                   ADD     1               TO  SPA-GMN-IDX
               ELSE
                   MOVE    SPA-MAX-LINE        TO  IDX3
               END-IF
           END-PERFORM
      *
           .
       410151-SETDEL2-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    セット削除処理
      *****************************************************************
       41016-SETDEL-SEC              SECTION.
      *
           COMPUTE IDX2                =   SPA-GMN-IDX   +   1
      *
           PERFORM VARYING     IDX     FROM    IDX2    BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
               IF      SPA-COM-SET     (IDX)   =   "1"
                   MOVE    SPACE               TO  SPA-GMN-INPUTCD(IDX)
               ELSE
                   MOVE    SPA-MAX-LINE            TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
                   MOVE    WRK-COM-TBLREC (IDX) 
                                           TO  SPA-COM-TBLREC
                                                        (SPA-GMN-MAX)
                   MOVE    WRK-GMN-TBLREC (IDX) 
                                           TO  SPA-GMN-TBLREC
                                                       (SPA-GMN-MAX)
               END-IF
           END-PERFORM
      *
           .
       41016-SETDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    １行削除処理
      *****************************************************************
       420-SYOKYO-SEC              SECTION.
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    SPACE               TO  FLG-SYORI
           MOVE    ZERO                TO  FLG-ZAIEND
           MOVE    ZERO                TO  FLG-DELFLG
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   IF      WRK-COM-TIMEFLG (IDX)   NOT =   ZERO
                       MOVE    1               TO  FLG-DELFLG
                       IF      WRK-COM-SRYKBN (IDX)    =   "11"  OR
                                                           "12"
                            MOVE    2               TO  FLG-DELFLG
                       END-IF
                   END-IF
                   IF     (WRK-COM-ZAIEND (IDX)    NOT =   "1" )  AND
                          (WRK-COM-DATAKBN(IDX)        =   "1"  OR
                           WRK-COM-INPUTCD(IDX)        =   SPACE)
                       MOVE    1               TO  FLG-ZAIEND
                   END-IF
      *            器材商品名
                   IF     (WRK-COM-INPUTCD(IDX)(1:3)   =   "058")
                       MOVE    2               TO  FLG-ZAIEND
                   END-IF
      *            前も空白の時対象行のみ
                   IF     (WRK-COM-INPUTCD(IDX)        =   SPACE )  AND
                          (WRK-MAE-INPUTCD(IDX)        =   SPACE )
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
      *            前が器材商品名
                   IF     (WRK-COM-AUTOKBN2(IDX)   NOT =   SPACE )
                      AND (WRK-COM-INPUTCD(IDX - 1)(1:3)
                                                       =   "058")
                       MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                             (IDX - 1)
                   END-IF
      *
                   IF     (WRK-COM-ZAIEND (IDX)    =   "1" )  OR
                         ((IDX                     <   SPA-MAX-LINE) AND
                          (WRK-GMN-SRYKBN (IDX + 1)    NOT =   SPACE))
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
               ELSE
                   IF      FLG-ZAIEND          =   1
                       IF      WRK-COM-AUTOKBN(IDX)    NOT =   SPACE
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                                 (IDX)
                       END-IF
                   END-IF
      *            前が器材商品名
                   IF      FLG-ZAIEND          =   2
                       IF      WRK-COM-AUTOKBN2(IDX)   NOT =   SPACE
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                                 (IDX)
                           MOVE    ZERO            TO  FLG-ZAIEND
                       END-IF
                   END-IF
      *            診療の時間加算削除の時、自動追加した時間加算を削除
                   IF      FLG-DELFLG          =   2
                       IF     (WRK-COM-AUTOKBN(IDX)    NOT =   SPACE)
                         AND  (WRK-COM-TIMEKSNKBN(IDX) NOT =   ZERO )
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                                (IDX)
                       END-IF
                   END-IF
                   IF     (WRK-COM-ZAIEND (IDX)    =   "1" )  OR
                         ((IDX                 <   SPA-MAX-LINE )  AND
                          (WRK-GMN-SRYKBN (IDX + 1)    NOT =   SPACE))
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-DELFLG      NOT =   ZERO
               MOVE    ZERO                TO  SPA-NAI-TIMEFLG
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   CONTINUE
               ELSE
                   MOVE    ZERO                TO  FLG-ERR
      *            登録前チェックの時、診療種別のみの剤は削除する
                   IF      FLG-TOROKU          =   "1"
                       IF      WRK-GMN-INPUTCD (IDX)(1:1)  =  "."
                           IF      (IDX            =   SPA-MAX-LINE ) OR
                                   (WRK-GMN-INPUTCD(IDX + 1)(1:1)
                                                   =   "."     )  OR
                                   (WRK-COM-INPUTCD(IDX + 1)
                                                   =   SPACE  AND
                                    WRK-GMN-INPUTCD(IDX + 1)
                                                   =   SPACE   )  OR
                                   (WRK-GMN-SRYKBN (IDX + 1)
                                                   NOT =   SPACE )
                               MOVE    1               TO  FLG-ERR
                           END-IF
                       END-IF
                   END-IF
      *
                   IF      FLG-ERR             =   ZERO
      *
                       ADD     1               TO  SPA-GMN-MAX
                       MOVE    WRK-COM-TBLREC (IDX) 
                                               TO  SPA-COM-TBLREC
                                                        (SPA-GMN-MAX)
                       MOVE    WRK-GMN-TBLREC (IDX) 
                                               TO  SPA-GMN-TBLREC
                                                       (SPA-GMN-MAX)
                       IF      SPA-GMN-MAX     NOT =   IDX
                           MOVE    "1"             TO  FLG-SYORI
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       420-SYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療チェック処理
      *****************************************************************
       5109-HKTSANTEI-CHK-SEC              SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO   SPA-WORK
           INITIALIZE                  ORCSCHKTCHKAREA
           MOVE    "1"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "KT01"              TO  ORCSCHKTCHK-PG
      *    算定履歴からの包括を行なう
           PERFORM 51021-ORCSCKAISU-HEN-SEC
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           IF      ORCSCHKTCHK-HKTFLG  =   1
               MOVE    1               TO  SPA-HKTSANTEI-CHK
           ELSE
               MOVE    ZERO            TO  SPA-HKTSANTEI-CHK
           END-IF
           IF      ORCSCHKTCHK-HKTERR  =   1
               MOVE    "0150"          TO  SPA-KIDCD
           END-IF
           .
       5109-HKTSANTEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    算定回数編集処理
      *****************************************************************
       51021-ORCSCKAISU-HEN-SEC             SECTION.
      *
           INITIALIZE                  ORCSCHKTCHK-KT01-AREA
           MOVE    "1"                 TO  ORCSCHKTCHK-KT01FLG
      *
           MOVE    SPA-SRYYMD(7:2)     TO  IDD
      *
           MOVE    ZERO                TO  IDX-SN2
           PERFORM VARYING     IDX-SN      FROM    1   BY  1
                   UNTIL      (IDX-SN      >   100     )   OR
                              (SPA-SAN-SRYCD  (IDX-SN) =   SPACE)
               IF     (SPA-SAN-DAY-G (IDX-SN)  =   ZERO )
               AND    (SPA-SANE-DAY-G (IDX-SN) =   ZERO )
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX-SN2
                   MOVE    SPA-SAN-SRYCD (IDX-SN)
                                           TO  ORCSCHKTCHK-KT01-SRYCD
                                                              (IDX-SN2)
                   PERFORM VARYING     IDX-N2  FROM    1   BY  1
                           UNTIL       IDX-N2  >   31
                       IF      IDX-N2          <   IDD
                           ADD     SPA-SAN-DAY (IDX-SN IDX-N2)
                                               TO  ORCSCHKTCHK-KT01-DAY
                                                       (IDX-SN2 IDX-N2)
                       END-IF
                       ADD     SPA-SANE-DAY (IDX-SN IDX-N2)
                                               TO  ORCSCHKTCHK-KT01-DAY
                                                       (IDX-SN2 IDX-N2)
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       51021-ORCSCKAISU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療エラー削除処理
      *****************************************************************
       51091-HKTSANTEI-DEL-SEC              SECTION.
      *
           INITIALIZE                  ORCSCHKTCHKAREA
           MOVE    "3"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "KT01"              TO  ORCSCHKTCHK-PG
           MOVE    LNK-ORCSC-KT01-AREA TO  ORCSCHKTCHK-KT01-AREA
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
      *    診療行為、計計算処理、編集
           MOVE    SPACE               TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           .
       51091-HKTSANTEI-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               PERFORM 210-BACK-SYORI-SEC
           ELSE
      *        ガイドメッセージ表示へ
               MOVE    "0104"              TO  SPA-KIDCD
               PERFORM 520-KIDSET-SEC
           END-IF
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC               SECTION.
      *
      *    排他削除
           MOVE    3                   TO  SLOCK-MODE
           MOVE    ZERO                TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING   MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
      *
           PERFORM 210-PARA-DEL-SEC
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    "M00"               TO  SPA-SAKIPG
           MOVE    "KT01"              TO  SPA-MOTOPG
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻るのクリア編集処理
      *****************************************************************
       210-PARA-DEL-SEC               SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID1
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME1          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME2          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID22
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM22
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME22         TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID3
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM3
      *    PERFORM VARYING    IDD      FROM    1   BY  1
      *            UNTIL      IDD      >   31
      *        MOVE    IDD                 TO  FILE-DAY
      *        MOVE    ZERO                TO  IF-RESULT
      *        MOVE    FILE-NAME3          TO  IF-FILENAME
      *        CALL    "orcfiledel"        USING
      *                                    ORCSFDELAREA
      *    END-PERFORM
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID4
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM4
      *    PERFORM VARYING    IDD      FROM    1   BY  1
      *            UNTIL      IDD      >   31
      *        MOVE    IDD                 TO  FILE-DAY4
      *        MOVE    ZERO                TO  IF-RESULT
      *        MOVE    FILE-NAME4          TO  IF-FILENAME
      *        CALL    "orcfiledel"        USING
      *                                    ORCSFDELAREA
      *****END-PERFORM
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "KT01"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "KT01"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
      *??
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "KTDA"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    1                   TO  FLG-END2
      *
           .
       210-PARA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理（確定）
      *****************************************************************
       210-BACK-OK-SEC                SECTION.
      *
           PERFORM 210-PARA-DEL-SEC
      *
      *    排他削除
           MOVE    3                   TO  SLOCK-MODE
           MOVE    ZERO                TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING   MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
      *
           MOVE    SPACE               TO  LINKAREA
           INITIALIZE                  SPA-KYOTUKEY
      **** MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "M00"               TO  SPA-SAKIPG
           MOVE    "KT01"              TO  SPA-MOTOPG
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-OK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者取消処理
      *****************************************************************
       222-PTNUMCLEAR-SEC             SECTION.
      *
      *     クリア確認チェック
           IF      (SPA-CLEARCHK-FLG   =   "1"   )  AND
                   (SPA-GMN-PTID   NOT =   ZERO  )
               AND (SPA-GMN-MAX        >   ZERO  )
               MOVE    "0125"              TO  SPA-KIDCD
               PERFORM 520-KIDSET-SEC
           ELSE
               PERFORM 2221-PTNUMCLEAR-SYORI-SEC
           END-IF
      *
           .
       222-PTNUMCLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者取消処理
      *****************************************************************
       2221-PTNUMCLEAR-SYORI-SEC             SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
           END-IF
      *
           PERFORM 30010-INIT-GMN-SEC
      *
           .
       2221-PTNUMCLEAR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEAR-SEC                   SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        確認メッセージ
               MOVE    "0116"              TO  SPA-KIDCD
           END-IF
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEAR-SYORI-SEC            SECTION.
      *
      *
           INITIALIZE              SPA-SCR-REC
      *
           MOVE    ZERO            TO  SPA-GMN-GOKEI
      *
           MOVE    ZERO            TO  SPA-NAI-TIMEFLG
      *
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-GMN-PAGE
           .
       220-CLEAR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    中間ワーク診療行為クリア処理
      *****************************************************************
       4900-WKSRYACT-CLEAR-SEC             SECTION.
      *
           PERFORM 210-PARA-DEL-SEC
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID3
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM3
      *    PERFORM VARYING    IDD      FROM    1   BY  1
      *            UNTIL      IDD      >   31
      *        MOVE    IDD                 TO  FILE-DAY
      *        MOVE    ZERO                TO  IF-RESULT
      *        MOVE    FILE-NAME3          TO  IF-FILENAME
      *        CALL    "orcfiledel"        USING
      *                                    ORCSFDELAREA
      *    END-PERFORM
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID4
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM4
      *    PERFORM VARYING    IDD      FROM    1   BY  1
      *            UNTIL      IDD      >   31
      *        MOVE    IDD                 TO  FILE-DAY4
      *        MOVE    ZERO                TO  IF-RESULT
      *        MOVE    FILE-NAME4          TO  IF-FILENAME
      *        CALL    "orcfiledel"        USING
      *                                    ORCSFDELAREA
      *    END-PERFORM
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME2          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                    ORCSFDELAREA
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID22
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM22
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME22         TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                    ORCSFDELAREA
      *
           .
       4900-WKSRYACT-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外院内ボタン処理
      *****************************************************************
       4101-INGAI-SYORI-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-INGAI
      *        院外へ
               WHEN    "T"
                   MOVE    "F"             TO  SPA-GMN-INGAI
                   MOVE    "0"             TO  SPA-INGAIKBN
      *        院内 
               WHEN    "F"
                   MOVE    "T"             TO  SPA-GMN-INGAI
                   MOVE    "1"             TO  SPA-INGAIKBN
           END-EVALUATE
      *
      *    コード振替え
           PERFORM 4101-INGAI-HENKOU-SEC
      *
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           .
       4101-INGAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児科外来診察料、老人外総診断の算定変換処理
      *****************************************************************
       4101-INGAI-HENKOU-SEC             SECTION.
      *
      *    院外院内振替え処理
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSCKT10SAREA
           MOVE    "4"                 TO  ORCSCKT10S-SYORI
           CALL    "ORCSCKT10S"        USING
                                       ORCSCKT10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-KT01
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           .
       4101-INGAI-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴更新画面　処理
      *****************************************************************
       430-SANTEIRRK-SEC             SECTION.
      *
      *    患者存在チェック
           IF      SPA-PTID            =   ZERO
               MOVE    "1005"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      430-SANTEIRRK-EXT
           END-IF
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    "1006"          TO  SPA-ERRCD
               MOVE    00              TO  SPA-GMN-CUR
               MOVE    3               TO  SPA-HKN-ERR
               GO      TO      430-SANTEIRRK-EXT
           END-IF
      *
      *    診療行為入力チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )
               GO      TO      430-SANTEIRRK-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-KIDCD2
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
      **** MOVE    "KT03"              TO  SPA-SAKIPG
           MOVE    "K06"               TO  SPA-SAKIPG
           MOVE    "KT01"              TO  SPA-MOTOPG
           MOVE    "KT01"              TO  SPA-MOTOPG2
      *
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
      ***  MOVE    "KT03"              TO  MCP-WINDOW
           MOVE    "K06"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       430-SANTEIRRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計カードへ
      *****************************************************************
       450-KAIKEI-SEC             SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        診療行為入力チェック
               MOVE    1                   TO  FLG-HENKOU
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )
                   GO      TO      450-KAIKEI-EXT
               END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "KT01"              TO  SPA-MOTOPG
           MOVE    "KT01"              TO  SPA-MOTOPG2
           MOVE    "J02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *    INITIALIZE                      SPA-WORK
      *    MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
      *    MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "J02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       450-KAIKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納登録へ
      *****************************************************************
       460-SYUNOU-SEC             SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        診療行為入力チェック
               MOVE    1                   TO  FLG-HENKOU
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )
                   GO      TO      460-SYUNOU-EXT
               END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "KT01"              TO  SPA-MOTOPG
           MOVE    "KT01"              TO  SPA-MOTOPG2
           MOVE    "S02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *    INITIALIZE                      SPA-WORK
      *    MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
      *    MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "S02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       460-SYUNOU-EXT.
           EXIT.
      *****************************************************************
      *    病名登録　処理
      *****************************************************************
       440-BYOTOROKU-SEC             SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        診療行為入力チェック
               MOVE    1                   TO  FLG-HENKOU
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )
                   GO      TO      440-BYOTOROKU-EXT
               END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "KT01"              TO  SPA-MOTOPG
           MOVE    "KT01"              TO  SPA-MOTOPG2
           MOVE    "C02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "C02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       440-BYOTOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *   ワーク行為負担パラメタを作成　処理
      *****************************************************************
       4410-WSINPARA-SEC             SECTION.
      *
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    SPA-GMN-KANANAME    TO  SPA-KANANAME
           MOVE    SPA-GMN-SEX         TO  SPA-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAYW
           MOVE    SPA-NAI-BIRTHDAY    TO  SPA-BIRTHDAY
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
      *
           MOVE    SPA-SRYYMD          TO  WRK-SYMD
           PERFORM 520-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-SRYYMDW
      *
           INITIALIZE                  ORCSCKTWKSRYAREA
           MOVE    "5"                 TO  ORCSCWKSRY-KBN
           MOVE    "KT01"              TO  ORCSCWKSRY-PG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCKTWKSRY"      USING
                                       ORCSCKTWKSRYAREA
                                       SPA-AREA
                                       SPA-KT01
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           .
       4410-WSINPARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       450-MAEGMN-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-HENKOU
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )
             OR   (FLG-END             =   1     )
               GO      TO      450-MAEGMN-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE        =   1
               MOVE    1               TO  SPA-GMN-PAGE
           ELSE
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE   -   1
           END-IF
      *
           MOVE    1                   TO  SPA-MAP-IDX
           MOVE    99                  TO  SPA-GMN-CUR
           .
       450-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       460-ATOGMN-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-HENKOU
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      460-ATOGMN-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE        =   MAX-PAGE2
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "7002"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               COMPUTE WRK-FREE            =   SPA-GMN-PAGE  *  MAX-GMN
               IF      SPA-GMN-INPUTCD(WRK-FREE)   =   SPACE
                   CONTINUE
               ELSE
      *            次ページ表示
                   ADD     1                   TO  SPA-GMN-PAGE
                   IF      SPA-GMN-PAGE        =   MAX-PAGE
                       MOVE    "7003"              TO  SPA-ERRCD
                   END-IF
                   MOVE    88                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       460-ATOGMN-EXT.
           EXIT.
      *****************************************************************
      *    前受診履歴　処理
      *****************************************************************
       4109-B05-SYORI-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-NEXT
      *    確定　処理
           PERFORM 4110-KAKUTEI-SEC
           IF     (FLG-DAY-IN          =   1     )
             AND ((SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE ))
               GO      TO      4109-B05-SYORI-EXT
           END-IF
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-KIDCD
      *
           MOVE    WRK-SRYDD           TO  SPA-NAI-SRYDD
           MOVE    SPA-NAI-SRYDD       TO  IDD
           COMPUTE IDD                 =   IDD   -  1
           MOVE    ZERO                TO  WRK-DAY
           IF      IDD                 >   ZERO
               PERFORM VARYING     IDX     FROM    IDD      BY  -1
                   UNTIL      (IDX     <   1   )
                       OR     (WRK-DAY NOT =   ZERO )
                   IF      SPA-NAI-RRKDAY (IDX)    NOT =   ZERO
                       MOVE    IDX                 TO  WRK-DAY
                   END-IF
               END-PERFORM
           END-IF
           IF      WRK-DAY             =   ZERO
               PERFORM VARYING     IDX     FROM    31   BY  -1
                       UNTIL      (IDX     <   1  )
                           OR     (WRK-DAY NOT =   ZERO )
                   IF      SPA-NAI-RRKDAY (IDX)    NOT =   ZERO
                       MOVE    IDX                 TO  WRK-DAY
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-DAY         NOT =   ZERO
               MOVE    "3"                 TO  WRK-SYORI-KBN
               PERFORM 6001-DAYCHEK-SEC
           END-IF
      *
           .
       4109-B05-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次受診履歴 処理
      *****************************************************************
       4109-B08-SYORI-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-NEXT
      *    確定　処理
           PERFORM 4110-KAKUTEI-SEC
           IF     (FLG-DAY-IN          =   1     )
             AND ((SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE ))
               GO      TO      4109-B08-SYORI-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    WRK-SRYDD           TO  SPA-NAI-SRYDD
           MOVE    SPA-NAI-SRYDD       TO  IDD
           COMPUTE IDD                 =   IDD   +  1
           MOVE    ZERO                TO  WRK-DAY
           PERFORM VARYING     IDX     FROM    IDD      BY  1
                   UNTIL      (IDX     >   31  )
                       OR     (WRK-DAY NOT =   ZERO )
               IF      SPA-NAI-RRKDAY (IDX)    NOT =   ZERO
                   MOVE    IDX                 TO  WRK-DAY
               END-IF
           END-PERFORM
           IF      WRK-DAY             =   ZERO
               PERFORM VARYING     IDX     FROM    1        BY  1
                       UNTIL      (IDX     >   31  )
                           OR     (WRK-DAY NOT =   ZERO )
                   IF      SPA-NAI-RRKDAY (IDX)    NOT =   ZERO
                       MOVE    IDX                 TO  WRK-DAY
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-DAY         NOT =   ZERO
               MOVE    "3"                 TO  WRK-SYORI-KBN
               PERFORM 6001-DAYCHEK-SEC
           END-IF
      *
           .
       4109-B08-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診日選択
      *****************************************************************
       4104-BDAY-SYORI-SEC            SECTION.
      *
           MOVE    SPA-NAI-SRYDD       TO  WRK-MAE-SRYDD
      *
           MOVE    ZERO                TO  WRK-BDAY
           IF     (MCP-WIDGET(1:2)     =   "BD" ) AND
                  (MCP-WIDGET(3:2)     NUMERIC  )
               MOVE    MCP-WIDGET(3:2)     TO  WRK-BDAY
      *        受診済みの日は選択不可
               IF     (SPA-NAI-LSTDD       >   ZERO    )
                  AND (SPA-NAI-LSTDD       >=  WRK-BDAY )
                   GO      TO  4104-BDAY-SYORI-EXT
               END-IF
      *
               IF     (SPA-GMN-MAX         =   ZERO )  AND
                      (SPA-NAI-RRKDAY-G    =   ZERO )
                   MOVE    WRK-BDAY            TO  WRK-DAY
                   MOVE    WRK-DAY             TO  SPA-NAI-SRYDD
                   MOVE    WRK-DAY             TO  SPA-GMN-SRYDD
      *            受診内容がない時、再編集
                   PERFORM 410171-SRYYMD-INIT-SEC
      *!!
                   IF      SPA-ERRCD           =   "6008"
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
      *!!
               ELSE
      *            確定　処理
                   PERFORM 4110-KAKUTEI-SEC
                   IF     (FLG-DAY-IN          =   1     )
                     AND ((SPA-ERRCD       NOT =   SPACE )  OR
                          (SPA-KIDCD       NOT =   SPACE ))
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  SPA-ERRCD
                       MOVE    SPACE               TO  SPA-KIDCD
                       MOVE    WRK-BDAY            TO  WRK-DAY
                       MOVE    WRK-DAY             TO  SPA-NAI-SRYDD
                       MOVE    WRK-DAY             TO  SPA-GMN-SRYDD
                       MOVE    "3"                 TO  WRK-SYORI-KBN
                       PERFORM 6001-DAYCHEK-SEC
                       MOVE    1                  TO  SPA-GMN-CUR
      *                入院判定
                       PERFORM 3109-NYUIN-CHK-SEC
                       IF      SPA-GMN-MAX         =   ZERO
                           PERFORM 4101712-HKNCOMBI-CHK-SEC
                       END-IF
                   END-IF
      *!!
                   IF      SPA-ERRCD           =   "6008"
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
      *!!
               END-IF
           END-IF
           .
       4104-BDAY-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    前月処理
      *****************************************************************
       4106-B06S-SYORI-SEC             SECTION.
      *
           IF      SPA-NAI-SRYYM       <=  "200804"
               MOVE    "6004"              TO  SPA-ERRCD
           ELSE
      *        １ヵ月前
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-NAI-SRYYMD      TO  LNK-DAY6-KIJUN
               MOVE    "01"                TO  LNK-DAY6-KIJUN(7:2)
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    -1                  TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  SPA-NAI-SRYYMD
      *
               PERFORM 410171-SRYYMD-INIT-SEC
           END-IF
           .
       4106-B06S-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    次月処理
      *****************************************************************
       4107-B07S-SYORI-SEC             SECTION.
      *
      *    １ヵ月前
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-SRYYMD      TO  LNK-DAY6-KIJUN
           MOVE    "01"                TO  LNK-DAY6-KIJUN(7:2)
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    1                   TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  SPA-NAI-SRYYMD
      *
           PERFORM 410171-SRYYMD-INIT-SEC
           .
       4106-B06S-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料自動発生処理へ
      *****************************************************************
       4104-B04S-SYORI-SEC            SECTION.
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               CONTINUE
           ELSE
      *        診療行為入力チェック
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD           =   SPACE  )  OR
                      (SPA-KIDCD           =   SPACE  )
                   MOVE    "3001"          TO  SPA-KTID2CD
               END-IF
           END-IF
      *
           .
       4104-B04S-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＤＯ処理へ
      *****************************************************************
       480-DOSYORI-SEC            SECTION.
      *
      *    患者存在チェック
           IF      SPA-PTID            =   ZERO
               MOVE    "1005"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      480-DOSYORI-EXT
           END-IF
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    "1006"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-HKN-ERR
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      480-DOSYORI-EXT
           END-IF
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
      *H28.6
      *    数量ゼロはＤＯへ
           IF      SPA-ERRCD           =   "A001"
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      480-DOSYORI-EXT
           END-IF
      *    行挿入行をなくす
           IF      FLG-INS             =   1
               PERFORM 420-SYOKYO-SEC
           END-IF
      *
           MOVE    1                   TO  SPA-NAI-DOSELNUM
           MOVE    1                   TO  SPA-NAI-SELNUM
      *    ＤＯ検索処理へ
           PERFORM 4801-KT012-SYORI-SEC
           .
       480-DOSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ処理へ
      *****************************************************************
       4801-KT012-SYORI-SEC            SECTION.
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-KT01            TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGKT012"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-KT01
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    "KT012"             TO  SPA-SAKIPG
           MOVE    "KT01"              TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KT012"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4801-KT012-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回患者番号編集処理
      *****************************************************************
       230-MAEKPTNUM-SEC             SECTION.
      *
           IF      SPA-LAST-PTNUM      =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-LAST-PTNUM      TO  KT01-PTNUM
               PERFORM 41010-PTNUM-SYORI-SEC
           END-IF
      *
           .
       230-MAEKPTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-P97-KENSAKU-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-KT01             TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "KT01"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名を設定
           MOVE    WRK-P97-NAME        TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-P97-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前の基本チェック処理
      *****************************************************************
       41001-KIHON-CHK-SEC             SECTION.
      *
      *    患者マスター存在チェック
           IF      SPA-PTNUM           =   SPACE
               MOVE    "1003"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF     (FLG-PTINF           =   1     )  OR
                  (SPA-PTNUM           =   SPACE )
               MOVE    "1003"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
           ELSE
               IF      SPA-HKNCOMBINUM     =   SPACE
                   MOVE    "1006"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-HKN-ERR
                   MOVE    00              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-RJN-ERR
      *            老人保険編集エラーがある時、エラーとする
                   WHEN    1
                       MOVE    "1027"              TO  SPA-ERRCD
      *            老人保険編集エラーがある時、エラーとする
                   WHEN    2
                       MOVE    "1070"              TO  SPA-ERRCD
      *            後期高齢者の保険がない時
                   WHEN    3
                       MOVE    "1076"              TO  SPA-ERRCD
      *            退職国保で６５歳以上
                   WHEN    9
                       MOVE    "1077"              TO  SPA-ERRCD
               END-EVALUATE
           END-IF
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-HKN-ERR
      *            保険エラーがある時、エラーとする
                   WHEN    1
                       MOVE    "1028"              TO  SPA-ERRCD
                   WHEN    2
                       MOVE    "1032"              TO  SPA-ERRCD
      *            保険未選択の場合、エラー
                   WHEN    3
                       MOVE    "1006"              TO  SPA-ERRCD
      *            保険未選択の場合、エラー
                   WHEN    4
                       MOVE    "1034"              TO  SPA-ERRCD
                       MOVE    5                   TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
      *    入院中
           IF      SPA-NAI-NYUIN-ERR   =   1
               MOVE    "6006"              TO  SPA-ERRCD
           END-IF
      *    当月受診済み
           IF      SPA-NAI-RRK-ERR     =   2
               MOVE    "6002"              TO  SPA-ERRCD
           END-IF
           IF      SPA-NAI-RRK-ERR     =   1
               IF      SPA-NAI-LSTDD       >=  SPA-GMN-SRYDD
                   MOVE    "6008"              TO  SPA-ERRCD
                   MOVE    8                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *        会計照会で受診履歴変更
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-JYURRKFLG   =   1
                   MOVE    "0178"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
      *    地方公費のみの場合、警告
           IF      SPA-NAI-KOHFLG      =   1
               IF      SPA-NAI-KOHFLG-ERR  =   ZERO
                   MOVE    "K034"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
                   GO      TO      41001-KIHON-CHK-EXT
               END-IF
           END-IF
      *    国保 前期高齢者変更チェック
           IF      SPA-NAI-ZENKIFLG      =   1
               IF      SPA-NAI-ZENKIFLG-ERR    =   ZERO
                   MOVE    "K171"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
                   GO      TO      41001-KIHON-CHK-EXT
               END-IF
           END-IF
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
      *
           .
       41001-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定（包括算定チェック）処理
      *****************************************************************
       4911-HKTSANTEI-CHEK-SEC             SECTION.
      *
      *    確定と同じ
           PERFORM 4110-KAKUTEI-SEC
      *    更新前の基本チェック
      *    PERFORM 41001-KIHON-CHK-SEC
      *    更新前のチェック
      *    MOVE    SPACE               TO  FLG-TOROKU
      *    包括診療チェック
      *    MOVE    ZERO                TO  SPA-HKTKEI-FLG
      *    MOVE    1                   TO  FLG-HKTCHK
      *    PERFORM 510-TBLHEN-SEC
      *    MOVE    SPACE               TO  FLG-TOROKU
           .
       4911-HKTSANTEI-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       2101-ALLDELETE-SEC             SECTION.
      *
           IF      SPA-NAI-RRK-ERR     =   ZERO
               MOVE    "6011"              TO  SPA-ERRCD
           ELSE
               MOVE    "3004"              TO  SPA-KIDCD
           END-IF
           .
       2101-ALLDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括削除　処理
      *****************************************************************
       21011-ALL-DELETE-SYORI-SEC             SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-GMN-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-GMN-HKNCOMBINUM TO  JYURRK-HKNCOMBINUM
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           INITIALIZE                  TBL-JYURRK-AREA
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  RRK-MAX
           PERFORM
               UNTIL   (FLG-JYURRK         =   1   )
                  OR   (SPA-ERRCD      NOT =   SPACE )
               IF      JYURRK-EDANUM       =   1
                   ADD     1                   TO  IDX
                   IF      IDX                 >   31
                       MOVE    "6013"              TO  SPA-ERRCD
                   ELSE
                       MOVE    JYURRK-REC     TO  TBL-JYURRK-REC (IDX)
                       MOVE    IDX            TO  RRK-MAX
                   END-IF
               END-IF
               IF      JYURRK-GRP-DENPNUM  NOT =   ZERO
                   MOVE    "6012"              TO  SPA-ERRCD
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key18"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    伝票番号毎に削除する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX          >   RRK-MAX  )
                       OR     (SPA-ERRCD    NOT =   SPACE)
               INITIALIZE                  SPA-TEI-AREA
               INITIALIZE                  ORCSCGAIRAIAREA
               MOVE    "2"                 TO  ORCSCGAIRAI-KBN
      *        受診履歴
               MOVE    TBL-JYURRK-SRYKA(IDX)   TO  SPA-SRYKA
               MOVE    TBL-JYURRK-SRYKA(IDX)   TO  SPA-OLD-SRYKA
               MOVE    TBL-JYURRK-SRYYMD(IDX)  TO  SPA-SRYYMD
               MOVE    TBL-JYURRK-RENNUM(IDX)  TO  SPA-RENNUM
      *R01.10  削除ログ用
               MOVE    TBL-JYURRK-DENPNUM(IDX) TO  SPA-DENPNUM
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               CALL    "ORCSCGAIRAI"       USING
                                           SPA-AREA
                                           ORCSCGAIRAIAREA
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
      *        患者番号決定初期処理
               PERFORM 41010-PTNUM-SYORI-SEC
           END-IF
           .
       21011-ALL-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       4110-KAKUTEI-SEC             SECTION.
      *
           MOVE    SPA-NAI-SRYDD       TO  WRK-SRYDD
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE ) 
               GO      TO      4110-KAKUTEI-EXT
           END-IF
      *
      *    特定疾患処方管理・長期投薬加算有無クリア
           MOVE    ZERO                TO  SPA-CHOKITOFLG
           MOVE    ZERO                TO  SPA-TOKUTEIFLG
      *    更新前のチェック
           MOVE    "1"                 TO  FLG-TOROKU
      *    包括診療チェック
           MOVE    ZERO                TO  FLG-HKTCHK
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
      *
           MOVE    ZERO                TO  FLG-DAY-IN
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   (IDX2       >   SPA-MAX-LINE )  OR
                           (FLG-DAY-IN =   1            )  OR
                          ((SPA-GMN-INPUTCD(IDX2)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX2)   =   SPACE ))
               IF      SPA-GMN-INPUTCD (IDX2)(1:1)  =   "*"
                   MOVE    1               TO  FLG-DAY-IN
               END-IF
           END-PERFORM
      *    診療日毎にテーブルを作成する
           IF     (SPA-ERRCD           =   SPACE )  OR
                  (SPA-KIDCD           =   SPACE )
               PERFORM 600-WKTBLHEN-SEC
           END-IF
      *
           IF     (FLG-NEXT            =   ZERO  )  AND
                 ((SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE ))
               IF      SPA-NAI-SRYDD   NOT =   WRK-SRYDD
                   MOVE    WRK-SRYDD           TO  SPA-GMN-SRYDD
                   MOVE    WRK-SRYDD           TO  SPA-NAI-SRYDD
                   MOVE    WRK-SRYDD           TO  WRK-DAY
                   MOVE    "3"                 TO  WRK-SYORI-KBN
                   PERFORM 6001-DAYCHEK-SEC
               END-IF
           END-IF
      *
           .
       4110-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       4100-TOROKU-SEC             SECTION.
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )
             OR   (FLG-KINKI           =   1     )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    特定疾患処方管理・長期投薬加算有無クリア
           MOVE    ZERO                TO  SPA-CHOKITOFLG
           MOVE    ZERO                TO  SPA-TOKUTEIFLG
      *    更新前のチェック
           MOVE    "1"                 TO  FLG-TOROKU
      *    包括診療チェック
           MOVE    1                   TO  FLG-HKTCHK
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    診療日毎にテーブルを作成する
           PERFORM 600-WKTBLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    明細がない時、エラー
           IF     (SPA-NAI-RRKDAY-G    =   ZERO )
               MOVE    "9301"          TO  SPA-ERRCD
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    後期高齢者で当月生活習慣病管理料算定中
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-SEIKATU     =   1
                   IF      SPA-NAI-SEIKATUCHK  =   ZERO
                       MOVE    "K177"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-NAI-SEIKATUCHK
                   END-IF
               END-IF
           END-IF
      *
      *    禁忌更新前のチェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41001-YAKKNK-CHK-SEC
           END-IF
      *
      *     禁忌エラーの時、エラー表示へ
           IF     (FLG-KINKI           =   1   )  AND
                  (FLG-END             =   1   )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
           MOVE    ZERO                TO  SPA-NAI-DAYFLG-G
           MOVE    ZERO                TO  FLG-KAKUNIN
      *    更新処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41002-KOUSIN-SYORI-SEC
           END-IF
           .
       4100-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       41002-KOUSIN-SYORI-SEC          SECTION.
      *
           IF      FLG-KAKUNIN         =   1
      *        確認ＯＫ処理
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                  ORCSC100AREA
               MOVE    "2"                 TO  ORCSC100-KBN
               MOVE    FLG-TOROKU          TO  ORCSC100-TOROKU
               MOVE    FLG-INIT            TO  ORCSC100-INIT
               MOVE    FLG-KENSADEL        TO  ORCSC100-KENSADEL
               MOVE    FLG-RIHTEI          TO  ORCSC100-RIHTEI
               CALL    "ORCSCKT100"        USING
                                           ORCSC100AREA
                                           SPA-AREA
                                           SPA-KT01
                                           SPA-SCR-REC
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               IF     (SPA-ERRCD           =   SPACE )  AND
                      (SPA-KIDCD           =   SPACE )
                   MOVE    1               TO  SPA-NAI-DAYFLG
                                                 (SPA-NAI-SRYDD)
               END-IF
           ELSE
      *        算定履歴削除
               PERFORM 60021-SANTEI-ALLDEL-SEC
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   31  )  OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (SPA-KIDCD   NOT =   SPACE )
               IF     (SPA-NAI-RRKDAY (IDX)    NOT =   ZERO )
                AND   (SPA-NAI-DAYFLG (IDX)        =   ZERO )
                   MOVE    IDX                 TO  WRK-DAY
                   PERFORM 6002-ALL-DAYCHEK-SEC
                   IF     (SPA-ERRCD           =   SPACE )  AND
                          (SPA-KIDCD           =   SPACE )
                       MOVE    1               TO  SPA-NAI-DAYFLG(IDX)
                   END-IF
               END-IF
           END-PERFORM
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *
      *!!!!!!
      *    再度、排他のチェック
           IF      SPA-LOCK            =   ZERO
               MOVE    1                   TO  SLOCK-MODE
               MOVE    SPA-PTID            TO  SLOCK-PTID
               CALL    "ORCSLOCK"          USING
                                           MCPAREA
                                           SPA-AREA
                                           ORCSLOCKAREA
               MOVE    ZERO            TO  SPA-LOCK
               IF      SLOCK-RETURN        =   10
      *            排他中
                   MOVE    "9997"              TO  SPA-ERRCD
                   GO      TO      41002-KOUSIN-SYORI-EXT
               END-IF
           END-IF
      *!!!!!!
      *
      *    受診日設定
           INITIALIZE                  SPA-KT01-RRKDAY-G
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   31  ) 
               IF      SPA-NAI-RRKDAY (IDX)    NOT =   ZERO
                   ADD     1               TO  IDY
                   MOVE    IDX             TO  SPA-KT01-RRKDAY (IDY)
                   MOVE    IDY             TO  SPA-KT01-RRKMAX
               END-IF
           END-PERFORM
           MOVE    SPA-KT01-RRKDAY(1)  TO  SPA-NAI-SRYDD
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
           MOVE    SPA-NAI-SRYDD       TO  SPA-SRYYMDW(8:2)
      *
           MOVE    "KT02"              TO  SPA-SAKIPG
           MOVE    "KT01"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-KT01-KYOUTU     TO  SPA-ETCKYOUTU-AREA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           IF      WRK-PUTTYPE         =   SPACE
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       41002-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日毎にテーブルを作成する
      *****************************************************************
       6002-ALL-DAYCHEK-SEC            SECTION.
      *
           INITIALIZE                  SPA-SCR-REC
           MOVE    WRK-DAY             TO  SPA-GMN-SRYDD
           MOVE    WRK-DAY             TO  SPA-NAI-SRYDD
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
      *    年齢計算
           PERFORM 3102-AGECHK-HEN-SEC
      *
           INITIALIZE                  ORCSCKTWKSRYAREA
           MOVE    "1"                 TO  ORCSCWKSRY-KBN
           MOVE    WRK-DAY             TO  ORCSCWKSRY-DAY
           MOVE    "KT01"              TO  ORCSCWKSRY-PG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCKTWKSRY"      USING
                                       ORCSCKTWKSRYAREA
                                       SPA-AREA
                                       SPA-KT01
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    SPACE               TO  WRK-ERRCD1
           MOVE    SPACE               TO  WRK-ERRCD3
           IF      ORCSCWKSRY-ERRCD    NOT =   SPACE
               MOVE    ORCSCWKSRY-ERRCD    TO  WRK-ERRCD1
           END-IF
           IF      ORCSCWKSRY-ERRCD2   NOT =   SPACE
               MOVE    ORCSCWKSRY-ERRCD2   TO  WRK-ERRCD3
           END-IF
      *    明細なし
           IF      SPA-GMN-MAX         =   ZERO
               PERFORM 6003-SANTEI-DEL-SEC
           ELSE
      *        警告は表示しない
               PERFORM 60011-KEIKOKU-SET-SEC
      *
               PERFORM 6003-SANTEI-DEL-SEC
      *        病名疾患区分編集
               PERFORM 60012-TOKUTEI-HEN-SEC
      *        初診料入力は日毎
               MOVE    ZERO                TO  SPA-SYOSIN
      *
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                  ORCSC100AREA
               MOVE    "3"                 TO  ORCSC100-KBN
               MOVE    FLG-TOROKU          TO  ORCSC100-TOROKU
               MOVE    FLG-INIT            TO  ORCSC100-INIT
               MOVE    FLG-KENSADEL        TO  ORCSC100-KENSADEL
               MOVE    FLG-RIHTEI          TO  ORCSC100-RIHTEI
               CALL    "ORCSCKT100"        USING
                                           ORCSC100AREA
                                           SPA-AREA
                                           SPA-KT01
                                           SPA-SCR-REC
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
               MOVE    2                   TO  FLG-SANTEI-SYORI
               PERFORM 6004-SANTEI-SYORI-SEC
      *        包括診療チェック
               IF     (SPA-ERRCD           =   SPACE )  AND
                      (SPA-KIDCD           =   SPACE )  AND
                      (SPA-HKNCOMBINUM NOT =   "9999")
                AND   (SPA-CHIKENKBN       =   SPACE )
                   IF     (SPA-HKTSANTEI-FLG   =   1    ) OR
                          (SPA-HKTSANTEI-08    =   1    )
                       PERFORM 5109-HKTSANTEI-CHK-SEC
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD           =   SPACE
                   MOVE    SPA-K01KYOUTU       TO  SPA-WORK
                   INITIALIZE                  ORCSC100AREA
                   MOVE    "2"                 TO  ORCSC100-KBN
                   MOVE    FLG-TOROKU          TO  ORCSC100-TOROKU
                   MOVE    FLG-INIT            TO  ORCSC100-INIT
                   MOVE    FLG-KENSADEL        TO  ORCSC100-KENSADEL
                   MOVE    FLG-RIHTEI          TO  ORCSC100-RIHTEI
                   CALL    "ORCSCKT100"        USING
                                               ORCSC100AREA
                                               SPA-AREA
                                               SPA-KT01
                                               SPA-SCR-REC
                   MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
             AND  (WRK-ERRCD1(1:1) NOT =   "K"   )
               MOVE    WRK-ERRCD1          TO  SPA-ERRCD
           END-IF
           IF     (SPA-ERRCD           =   SPACE )
             AND  (WRK-ERRCD3      NOT =   SPACE )
             AND  (WRK-ERRCD3(1:1) NOT =   "K"   )
               MOVE    WRK-ERRCD3          TO  SPA-ERRCD
           END-IF
           .
       6002-ALL-DAYCHEK-EXT.
           EXIT.
      *****************************************************************
      *    診療日毎にテーブルを作成する
      *****************************************************************
       600-WKTBLHEN-SEC            SECTION.
      *
           MOVE    "3"                 TO  WRK-SYORI-KBN
      *
           INITIALIZE                  ORCSCKTWKSRYAREA
           MOVE    "2"                 TO  ORCSCWKSRY-KBN
           MOVE    "KT01"              TO  ORCSCWKSRY-PG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCKTWKSRY"      USING
                                       ORCSCKTWKSRYAREA
                                       SPA-AREA
                                       SPA-KT01
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           IF     (FLG-DAY-IN          =   ZERO )
      *        日の指定がない時、診療日のみ
               MOVE    SPA-NAI-SRYDD       TO  WRK-DAY
               PERFORM 6001-DAYCHEK-SEC
           ELSE
      *
      *        算定履歴削除
               PERFORM 60021-SANTEI-ALLDEL-SEC
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   31  )  OR
                                  (SPA-ERRCD   NOT =   SPACE )
                   IF      SPA-NAI-RRKDAY (IDX)    NOT =   ZERO
                       MOVE    IDX                 TO  WRK-DAY
                       PERFORM 6001-DAYCHEK-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       600-WKTBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日毎診療行為チェック処理
      *****************************************************************
       6001-DAYCHEK-SEC           SECTION.
      *
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-GOKEI
      *
           MOVE    WRK-DAY             TO  SPA-GMN-SRYDD
           MOVE    WRK-DAY             TO  SPA-NAI-SRYDD
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
      *    年齢計算
           PERFORM 3102-AGECHK-HEN-SEC
      *
           INITIALIZE                  ORCSCKTWKSRYAREA
           MOVE    "1"                 TO  ORCSCWKSRY-KBN
           MOVE    WRK-DAY             TO  ORCSCWKSRY-DAY
           MOVE    "KT01"              TO  ORCSCWKSRY-PG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCKTWKSRY"      USING
                                       ORCSCKTWKSRYAREA
                                       SPA-AREA
                                       SPA-KT01
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    明細なし
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
               PERFORM 6003-SANTEI-DEL-SEC
           ELSE
      *        警告は表示しない
               PERFORM 60011-KEIKOKU-SET-SEC
      *
               PERFORM 6003-SANTEI-DEL-SEC
      *
               MOVE    WRK-DAY             TO  SPA-GMN-SRYDD
               MOVE    WRK-DAY             TO  SPA-NAI-SRYDD
               MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
      *        初診料入力は日毎
               MOVE    ZERO                TO  SPA-SYOSIN
      *
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                  ORCSC100AREA
               MOVE    WRK-SYORI-KBN       TO  ORCSC100-KBN
               MOVE    FLG-TOROKU          TO  ORCSC100-TOROKU
               MOVE    FLG-INIT            TO  ORCSC100-INIT
               MOVE    FLG-KENSADEL        TO  ORCSC100-KENSADEL
               MOVE    FLG-RIHTEI          TO  ORCSC100-RIHTEI
               CALL    "ORCSCKT100"        USING
                                           ORCSC100AREA
                                           SPA-AREA
                                           SPA-KT01
                                           SPA-SCR-REC
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *        算定回数登録
               MOVE    2                   TO  FLG-SANTEI-SYORI
               PERFORM 6004-SANTEI-SYORI-SEC
      *        包括診療チェック
               IF     (SPA-ERRCD           =   SPACE )  AND
                      (SPA-KIDCD           =   SPACE )  AND
                      (SPA-HKNCOMBINUM NOT =   "9999")
                 AND  (SPA-CHIKENKBN       =   SPACE )
                   IF     (SPA-HKTSANTEI-FLG   =   1    ) OR
                          (SPA-HKTSANTEI-08    =   1    )
                       PERFORM 5109-HKTSANTEI-CHK-SEC
                   END-IF
               END-IF
      *
           END-IF
           .
       6001-DAYCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    警告設定編集処理
      *****************************************************************
       60011-KEIKOKU-SET-SEC           SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   (IDX2       >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX2)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX2)   =   SPACE ))
      *        警告表示をしない
               MOVE    "1"                 TO  SPA-COM-KZMFLG  (IDX2)
               MOVE    "1"                 TO  SPA-COM-HKEIFLG (IDX2)
               MOVE    "1"                 TO  SPA-COM-KZMFLG2 (IDX2)
               MOVE    "1"                 TO  SPA-COM-KZMFLG3 (IDX2)
               MOVE    "1"                 TO  SPA-COM-KZMFLG4 (IDX2)
               MOVE    "1"                 TO  SPA-COM-KZMFLG5 (IDX2)
               MOVE    "1"                 TO  SPA-COM-KZMFLG6 (IDX2)
               MOVE    "1"                 TO  SPA-COM-KZMFLG7 (IDX2)
      *H31.4
               MOVE    "1"                 TO  SPA-COM-KZMFLG8 (IDX2)
               MOVE    "1"                 TO  SPA-COM-KZMFLG9 (IDX2)
               MOVE    "1"                 TO  SPA-COM-KZMFLG10(IDX2)
               MOVE    "1"                 TO  SPA-COM-KINKIFLG(IDX2)
               MOVE    "1"                 TO  SPA-COM-TUKIJGNFLG(IDX2)
               MOVE    "1"                 TO  SPA-COM-TUKIJGNFLG1(IDX2)
           END-PERFORM
           .
       60011-KEIKOKU-SET-EXT.
           EXIT.
      *****************************************************************
      *     病名疾患区分編集処理
      *****************************************************************
       60012-TOKUTEI-HEN-SEC       SECTION.
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "9"                 TO  LNK-SC95-KBN
           CALL    "ORCSCKT95"         USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-KT01
      *
           .
       60012-TOKUTEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴テーブル削除処理
      *****************************************************************
       60021-SANTEI-ALLDEL-SEC       SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   SPA-SANTEI-MAX)
               MOVE    ZERO                TO  SPA-SAN-DAY-G (IDX2)
               IF      SPA-SANE-DAY-G (IDX2)   =   ZERO
                   MOVE    SPACE           TO  SPA-SAN-SRYCD (IDX2)
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  SPA-SANTEI-MAX
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >=  100 )
               IF      SPA-SAN-SRYCD (IDX2)    =   SPACE
                   MOVE    SPA-SANTEI-OCC (IDX2 + 1)
                                       TO  SPA-SANTEI-OCC (IDX2)
                   MOVE    SPACE       TO  SPA-SAN-SRYCD (IDX2 + 1)
               END-IF
               IF      SPA-SAN-SRYCD(IDX2) NOT =   SPACE
                   ADD     1               TO  SPA-SANTEI-MAX
               END-IF
           END-PERFORM
           .
       60021-SANTEI-ALLDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴テーブル編集処理
      *****************************************************************
       6003-SANTEI-DEL-SEC           SECTION.
      *
           MOVE    WRK-DAY             TO  IDD2
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   SPA-SANTEI-MAX)
               MOVE    ZERO            TO  SPA-SAN-DAY (IDX2 IDD2)
           END-PERFORM
           .
       6003-SANTEI-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴テーブル編集処理
      *****************************************************************
       6004-SANTEI-SYORI-SEC          SECTION.
      *
           PERFORM VARYING     IDX-1   FROM    1   BY  1
                   UNTIL      (IDX-1   >   SPA-MAX-LINE    )   OR
                             ((SPA-GMN-INPUTCD (IDX-1) =   SPACE ) AND
                              (SPA-COM-INPUTCD (IDX-1) =   SPACE ))
                        OR    (SPA-HKNCOMBINUM         =   "9999")
               IF      (SPA-COM-INPUTCD (IDX-1)(1:1) =   "1"   )
                  OR   (SPA-COM-INPUTCD (IDX-1)(1:3) =   "099" )
                   IF      (SPA-COM-SRYKBN  (IDX-1)      =   "95"  OR 
                                                             "96"  )
                     OR    (SPA-COM-SANTEIRRKKBN (IDX-1) =   ZERO)
                       CONTINUE
                   ELSE
      *                算定履歴テーブル登録
                       PERFORM 6005-SANTEI-SET-SEC
                   END-IF
               END-IF
           END-PERFORM
          .
       6004-SANTEI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    算定回数処理
      *****************************************************************
       6005-SANTEI-SET-SEC             SECTION.
      *
      *    ＣＴ・ＭＲＩ
           IF       SPA-COM-CHK-SRYCD(IDX-1)   =   SPACE
               MOVE    SPA-COM-INPUTCD (IDX-1)     TO  WRK-HEN-SRYCD
           ELSE
               MOVE    SPA-COM-CHK-SRYCD(IDX-1)    TO  WRK-HEN-SRYCD
           END-IF
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   100  )
               IF      SPA-SAN-SRYCD (IDX2)    =   SPACE
                   MOVE    WRK-HEN-SRYCD       TO  SPA-SAN-SRYCD
                                                                 (IDX2)
                   MOVE    IDX2                TO  SPA-SANTEI-MAX
               END-IF
               IF      WRK-HEN-SRYCD       =   SPA-SAN-SRYCD (IDX2)
      *        画面内の回数を設定する
                   PERFORM VARYING IDX-N2      FROM    1   BY  1
                           UNTIL  (IDX-N2      >   31  )
                       IF      SPA-COM-NYUINDAY(IDX-1 IDX-N2)
                                               >   ZERO
                           ADD     SPA-COM-KAISU (IDX-1)
                                               TO   SPA-SAN-DAY
                                                           (IDX2 IDX-N2)
                       END-IF
                   END-PERFORM
                   MOVE    100             TO  IDX2
               END-IF
           END-PERFORM
           .
       6005-SANTEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌更新前のチェック処理
      *****************************************************************
       41001-YAKKNK-CHK-SEC            SECTION.
      *
      *    相互作用チェック期間
           MOVE    ZERO                TO  FLG-KINKI
           IF      SPA-INTERACT-KIKAN  >   ZERO
               INITIALIZE                  ORCSCKNKCHKAREA
               MOVE    "3"                 TO  ORCSCKNKCHK-KBN 
               CALL    "ORCSCKTKNKCHK"     USING
                                           ORCSCKNKCHKAREA
                                           SPA-AREA
                                           SPA-KT01
                                           SPA-SCR-REC
               IF      SPA-KT011-MAX        >   ZERO
                   MOVE    1                   TO  FLG-KINKI
                   MOVE    2                   TO  FLG-ALLFLG
                   MOVE    "KT02"              TO  SPA-KT011-SAKI
                   PERFORM 51020-KT011-SYORI-SEC
               END-IF
           END-IF
      *
           .
       41001-YAKKNK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌エラーメッセージ表示処理
      *****************************************************************
       51020-KT011-SYORI-SEC              SECTION.
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-KT01             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-KT01SPA-OUT-SEC
      *
           CALL    "ORCGKT011"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-KT01
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    "KT011"              TO  SPA-SAKIPG
           MOVE    "KT01"               TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KT011"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       51020-KT011-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       520-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       520-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
      *    老人保険編集エラーがある時、エラーとする
           IF      SPA-RJN-ERR         =   1
               MOVE    "1027"              TO  SPA-ERRCD
           END-IF
      *    老人保険編集エラーがある時、エラーとする
           IF      SPA-RJN-ERR         =   2
               MOVE    "1070"              TO  SPA-ERRCD
           END-IF
      *    後期高齢者の保険がない時、エラーとする
           IF      SPA-RJN-ERR         =   3
               MOVE    "1076"              TO  SPA-ERRCD
           END-IF
      *    退職国保の年齢
           IF      SPA-RJN-ERR         =   9
               MOVE    "1077"              TO  SPA-ERRCD
           END-IF
      *    保険エラーがある時、エラーとする
           IF      SPA-HKN-ERR         =   1
               MOVE    "1028"              TO  SPA-ERRCD
           END-IF
           IF      SPA-HKN-ERR         =   2
               MOVE    "1032"              TO  SPA-ERRCD
           END-IF
      *    保険未選択の場合、エラー
           IF      SPA-HKN-ERR         =   3
               MOVE    "1006"              TO  SPA-ERRCD
           END-IF
      *    保険選択エラーの場合
           IF      SPA-HKN-ERR         =   4
               MOVE    "1034"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *    会計照会で受診履歴変更
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-JYURRKFLG   =   1
                   MOVE    "0178"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    数量ゼロの場合、エラー表示しない
           IF      SPA-ERRCD           =   "A001"
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *
      *    訂正時の登録エラー
           IF     (SPA-TEISEI-ERR      =   1  )  AND
                  (SPA-TEISEI-CHK      =   ZERO)
               IF      SPA-ERRCD       NOT =   "9100"
                   MOVE    "K999"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-TEISEI-CHK
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    地方公費のみの場合、警告
           IF     (SPA-NAI-KOHFLG      =   1   )  AND
                  (SPA-ERRCD           =   SPACE
                                       OR   "K034")
               IF      SPA-NAI-KOHFLG-ERR  =   ZERO
                   MOVE    "K034"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-NAI-KOHFLG-ERR
               END-IF
           END-IF
      *    国保 前期高齢者変更チェック
           IF     (SPA-NAI-ZENKIFLG    =   1   )  AND
                  (SPA-ERRCD           =   SPACE
                                       OR   "K171")
               IF      SPA-NAI-ZENKIFLG-ERR    =   ZERO
                   MOVE    "K171"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-NAI-ZENKIFLG-ERR
               END-IF
           END-IF
      *    前回保険組合せの判定チェック
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-HKNCHK-FLG  NOT =   ZERO   )  AND 
                  (SPA-HKNCHK-CHK      =   ZERO   )
               IF      SPA-HKNCHK-FLG      =   2
                   MOVE    "K173"              TO  SPA-ERRCD
               ELSE
                   MOVE    "K172"              TO  SPA-ERRCD
               END-IF
               MOVE    5                   TO  SPA-GMN-CUR
               MOVE    1                   TO  SPA-HKNCHK-CHK
               MOVE    ZERO                TO  SPA-HKNCHK-FLG
           END-IF
      *
      *!!
      *    入院中
           IF      SPA-NAI-NYUIN-ERR   =   1
               MOVE    "6006"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        訂正時の保険自動変換
               IF      SPA-HKN-ERR2        =   1
                   MOVE    "K176"              TO  SPA-ERRCD
                   MOVE    ZERO                TO  SPA-HKN-ERR2
               END-IF
           END-IF
      *    初診料の併用算定判定
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-SYOSIN-KEI      =   1      )  AND
                  (SPA-SYOSIN-KEIFLG   =   ZERO   )
               MOVE    "K174"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-SYOSIN-KEIFLG
           END-IF
      *    初診料の併用算定判定
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-SYOSIN-KEI      =   2      )  AND
                  (SPA-SYOSIN-KEIFLG   =   ZERO   )
               MOVE    "K175"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-SYOSIN-KEIFLG
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-LOCK        =   2
                   MOVE    "9998"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-LOCK
               END-IF
           END-IF
      *
      *TEST
      *    コメント入力位置エラーはカーソル移動のみ
           IF      SPA-ERRCD           =   "0100"
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *TEST
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-KIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
           IF      SPA-KTID2CD     NOT =   SPACE
               PERFORM 520-KTID2SET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "KT01    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行位置とカーソル位置の決定処理
      *****************************************************************
       5002-GMNSPA-HEN-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-MAP-MAX
           MOVE    ZERO                TO  WRK-MAP-CUR
           MOVE    ZERO                TO  WRK-MAP-CUR2
           MOVE    ZERO                TO  WRK-MAP-CUR3
           MOVE    ZERO                TO  WRK-MAP-MAXPAGE
           MOVE    ZERO                TO  WRK-MAP-PAGE
           MOVE    ZERO                TO  WRK-MAP-PAGE2
           MOVE    ZERO                TO  WRK-MAP-PAGE3
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL       WRK-IDX     >   SPA-MAX-LINE
               MOVE    ZERO                TO  SPA-COM-PAGE (WRK-IDX)
               MOVE    ZERO                TO  SPA-COM-GYOU (WRK-IDX)
      *********MOVE    SPACE               TO  SPA-COM-IN3  (WRK-IDX)
      *        画面表示外のものに頁、行位置を設定する
               IF      SPA-COM-SET    (WRK-IDX)  =   SPACE
                   ADD     1                 TO  IDX
                   IF      IDX               >   MAX-GMN
                       ADD     1             TO  WRK-PAGE
                       MOVE    1             TO  IDX
                   END-IF
                   IF     (SPA-GMN-INPUTCD(WRK-IDX)  NOT =   SPACE) OR
                          (SPA-COM-CUR (WRK-IDX)     NOT =   SPACE)
                       MOVE    IDX             TO  WRK-MAP-MAX
                       MOVE    WRK-PAGE        TO  WRK-MAP-MAXPAGE
      *                前・次ページの時
                       IF     (SPA-GMN-CUR         =   88  OR  99 )
                           IF      SPA-COM-CUR (WRK-IDX)   =   "1" 
                              AND (SPA-ERRCD      NOT =   SPACE )
                               IF      WRK-MAP-CUR         =   ZERO
                                   MOVE    IDX        TO  WRK-MAP-CUR
                                   MOVE    WRK-PAGE   TO  WRK-MAP-PAGE
                               END-IF
                           END-IF
                       ELSE
      *                エラー位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "1" 
                                                       OR  "3"
                           IF      WRK-MAP-CUR         =   ZERO
                               MOVE    IDX            TO  WRK-MAP-CUR
                               MOVE    WRK-PAGE       TO  WRK-MAP-PAGE
                           END-IF
                       END-IF
      *                警告位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "2"
                           IF      WRK-MAP-CUR2        =   ZERO
                               MOVE    IDX             TO  WRK-MAP-CUR2
                               MOVE    WRK-PAGE        TO  WRK-MAP-PAGE2
                           END-IF
                       END-IF
      *                フリーコメントで未入力の行
                       IF     (SPA-COM-IN2 (WRK-IDX)   =   "3" ) AND
                              (SPA-COM-IN  (WRK-IDX)   =   "1" )
                           MOVE    IDX             TO  WRK-MAP-CUR3
                           MOVE    WRK-PAGE        TO  WRK-MAP-PAGE3
                       END-IF
      *
                       END-IF
                   END-IF
      *
                   MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
                   MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
      *
                   IF      SPA-COM-CUR (WRK-IDX)   =   "1"
      *!!
      *            セットで警告の時
                       IF     (SPA-GMN-INPUTCD (WRK-IDX)(1:1) =  "S")
                          AND (SPA-ERRCD(1:1)      =   "K"   )
                          AND ((SPA-COM-KZMFLG4 (WRK-IDX)  =  "1" )
                           OR  (SPA-COM-KZMFLG5 (WRK-IDX)  =  "1" )
                           OR  (SPA-COM-KINKIFLG(WRK-IDX)  =  "1" )
                           OR  (SPA-COM-TUKIJGNFLG(WRK-IDX)  =  "1" )
                           OR  (SPA-COM-TUKIJGNFLG1(WRK-IDX) =  "1" ))
                          CONTINUE
                       ELSE
                           MOVE    SPACE       TO  SPA-MAE-INPUTCD
                                                            (WRK-IDX)
                       END-IF
                   ELSE
                       IF      SPA-COM-CUR (WRK-IDX)   NOT =   "2"
                           MOVE    SPACE       TO  SPA-COM-CUR (WRK-IDX)
                       END-IF
                   END-IF
               END-IF
      *
               IF      (SPA-COM-INPUTCD(WRK-IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(WRK-IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
      *
      *    画面カーソル位置決定（警告はエラーの次に）
           MOVE    ZERO                TO  SPA-MAP-IDX
           IF      WRK-MAP-CUR         =   ZERO
               IF      WRK-MAP-CUR2    NOT =   ZERO
                   MOVE    WRK-MAP-CUR2        TO  SPA-MAP-IDX
                   MOVE    WRK-MAP-PAGE2       TO  SPA-GMN-PAGE
               END-IF
           ELSE
               MOVE    WRK-MAP-CUR         TO  SPA-MAP-IDX
               MOVE    WRK-MAP-PAGE        TO  SPA-GMN-PAGE
           END-IF
      *
           IF      SPA-MAP-IDX     NOT =   ZERO
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF
      *    名称入力へ
           IF      WRK-MAP-CUR3    NOT =   ZERO
               MOVE    WRK-MAP-CUR3        TO  SPA-MAP-IDX
               MOVE    WRK-MAP-PAGE3       TO  SPA-GMN-PAGE
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF
      *
      *    カーソル指定のない時、次ぎの空白行へ
      *    後画面へ
           IF      SPA-GMN-CUR         =   88  OR  99
               IF      WRK-MAP-MAXPAGE     =   SPA-GMN-PAGE
                   COMPUTE SPA-MAP-IDX         =   WRK-MAP-MAX
                                               +   1
               ELSE
                   MOVE    1                   TO  SPA-MAP-IDX
               END-IF
           ELSE
               COMPUTE SPA-MAP-IDX         =   WRK-MAP-MAX
                                           +   1
               IF      SPA-SINSATU-SAI     =   ZERO
      *            初期表示の時１ページ目へ
                   IF     (SPA-MAP-IDX         >   MAX-GMN )
                       OR (SPA-GMN-PAGE        <   WRK-MAP-MAXPAGE)
                       MOVE    99                  TO  SPA-MAP-IDX
                   ELSE
                       IF      SPA-GMN-PAGE    >   WRK-MAP-MAXPAGE
                           MOVE    WRK-MAP-MAXPAGE     TO  SPA-GMN-PAGE
                       END-IF
                   END-IF
               ELSE
                   MOVE    ZERO            TO  SPA-SINSATU-SAI
      *            最終行へ
                   IF      SPA-MAP-IDX         >   MAX-GMN
                       MOVE    99                  TO  SPA-MAP-IDX
                   ELSE
                       MOVE    WRK-MAP-MAXPAGE     TO  SPA-GMN-PAGE
                   END-IF
               END-IF
           END-IF
           IF      SPA-GMN-PAGE        =   ZERO
               MOVE    1               TO  SPA-GMN-PAGE
           END-IF
      *
           .
       5002-GMNSPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    行位置とカーソル位置の決定
           PERFORM 5002-GMNSPA-HEN-SEC
      *
           MOVE    SPACE               TO  KT01
           INITIALIZE                      KT01
      *
           MOVE    ZERO                TO  IDX
           MOVE    1                   TO  WRK-PAGE
      *H29.1
      *    院外投薬あり
           MOVE    ZERO                TO  FLG-INGAI-ARI
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL      (WRK-IDX     >   SPA-MAX-LINE)  OR
                              (IDX         >=  MAX-GMN )
      *        画面表示
               IF      SPA-COM-PAGE(WRK-IDX)   =   SPA-GMN-PAGE
                   ADD     1               TO  IDX
                   MOVE    SPA-GMN-SRYKBN (WRK-IDX)
                                               TO  KT01-SRYKBN(IDX)
                   MOVE    SPA-GMN-INPUTCD(WRK-IDX)
                                               TO  KT01-INPUTCD(IDX)
      *
      *TEST
      *H30.9       選択式コメントあり
                   IF      SPA-COM-RECEKISAI (WRK-IDX) NOT =  SPACE
                       MOVE   "S"       TO  SPA-GMN-MEIH (WRK-IDX)(2:1)
                   END-IF
      *
                   MOVE    SPA-GMN-MEISYO-G(WRK-IDX)
                                               TO  KT01-MEISYO(IDX)
                   PERFORM 5002-SURYO-HEN-SEC
      *H26.10
      *            診療人数合計
                   IF     (SPA-COM-INPUTCD (WRK-IDX)
                                                   =   "099140012" )
                       PERFORM 50021-MEISYOHEN-SEC
                   END-IF
      *@
      *            包括行
                   IF     (SPA-COM-HOUKATU-ZAI (WRK-IDX) NOT =   ZERO)
                      OR ((SPA-COM-HKNCOMBI(WRK-IDX)     =   "9999" )
                      AND (SPA-HKNCOMBINUM    NOT =   "9999" ))
                       MOVE    "blue"         TO  KT01-SRYKBN-STYLE(IDX)
                       MOVE    "blue"         TO  KT01-MEISYO-STYLE(IDX)
                       IF       SPA-COM-HOUKATU-FLG2(WRK-IDX)   =   1
                           MOVE    "red"      TO  KT01-SRYKBN-STYLE(IDX)
                           MOVE    "red"      TO  KT01-MEISYO-STYLE(IDX)
                       END-IF
                   ELSE
                       MOVE    "black"        TO  KT01-SRYKBN-STYLE(IDX)
                       MOVE    "black"        TO  KT01-MEISYO-STYLE(IDX)
                   END-IF
      *
      *!!          禁忌薬剤
                   IF      SPA-COM-PTKINKI-ARI (WRK-IDX)   =   1
                       MOVE    "red"          TO  KT01-MEISYO-STYLE(IDX)
                   END-IF
      *!
      *            IF      SPA-GMN-INPUTCD(WRK-IDX)(1:1)   =   "#"
      *                 MOVE    ALL "#"             TO  KT01-SRYKBN(IDX)
      *                 MOVE    ALL "#"             TO  KT01-SURYO (IDX)
      *            END-IF
      *!
      *            IF      SPA-GMN-INPUTCD(WRK-IDX)(1:1)   =   "$"
      *                 MOVE    ALL "="             TO  KT01-SRYKBN(IDX)
      *                 MOVE    ALL "="             TO  KT01-SURYO (IDX)
      *             END-IF
      *!
               END-IF
      *H29.1
      *        院外投薬あり
               IF      (SPA-COM-SRYSYUKBN(WRK-IDX) =   "212"
                                                   OR  "292"
                                                   OR  "222"
                                                   OR  "298"
                                                   OR  "232"
                                                   OR  "148"
                                                   OR  "295")
                   OR (SPA-COM-INGAITEN (WRK-IDX)  NOT =   ZERO)
                   MOVE    1               TO  FLG-INGAI-ARI
               END-IF
      *
           END-PERFORM
      *
      *    ページ表示
           MOVE    SPACE               TO  WRK-HEN-PAGE
           MOVE    "頁"                TO  WRK-HEN1
           IF      WRK-MAP-MAXPAGE     >   ZERO
               MOVE    SPA-GMN-PAGE        TO  WRK-HEN-PAGE1
               MOVE    "/"                 TO  WRK-HEN2
               MOVE    WRK-MAP-MAXPAGE     TO  WRK-HEN-PAGE2
           END-IF
           MOVE    WRK-HEN-PAGE        TO  KT01-MODE
      *
      *    前頁表示の時、１行目へ
           IF      SPA-GMN-CUR         =   99  OR  88
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
      *    明細入力の時、コードが入力されていること
           IF      SPA-GMN-CUR         =   2
               IF      KT01-INPUTCD(SPA-MAP-IDX)    =   SPACE
                   MOVE    1               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-PTNUM       TO  KT01-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  KT01-KANANAME
           MOVE    SPA-GMN-NAME        TO  KT01-NAME
      *    排他中
           IF      SPA-LOCK                =   ZERO
               MOVE    "black"             TO  KT01-KANANAME-STYLE
               MOVE    "black"             TO  KT01-NAME-STYLE
           ELSE
               MOVE    "red"               TO  KT01-KANANAME-STYLE
               MOVE    "red"               TO  KT01-NAME-STYLE
           END-IF
           MOVE    SPA-GMN-SEX         TO  KT01-SEX
           MOVE    SPA-GMN-HKNCOMBI-G
                                       TO  KT01-HKNCOMBI
           MOVE    SPA-GMN-FTNRATEMEI  TO  KT01-RATE
      *    診療日
           IF      SPA-GMN-SRYYM       =   SPACE
               MOVE    SPA-SRYYMDW(1:6)    TO  KT01-SRYYM
               MOVE    SPA-SRYYMD (7:2)    TO  KT01-SRYDD
           ELSE
               MOVE    SPA-GMN-SRYYM       TO  KT01-SRYYM
               MOVE    SPA-GMN-SRYDD       TO  KT01-SRYDD
           END-IF
           MOVE    SPA-GMN-BIRTHDAY    TO  KT01-BIRTHDAY
      *    年齢
           MOVE    SPA-NENREI-HEN      TO  KT01-NENREI
      *
           IF      SPA-GMN-GOKEI       >=  ZERO
               MOVE    SPA-GMN-GOKEI       TO  WRK-GOKEI-Z2
               MOVE    WRK-GOKEI-X2        TO  KT01-GOKEITEN
           ELSE
               MOVE    SPA-GMN-GOKEI       TO  WRK-GOKEI-Z3
               MOVE    WRK-GOKEI-X3        TO  KT01-GOKEITEN
           END-IF
           MOVE    SPA-GMN-LASTYMD-G   TO  KT01-LASTYMD
           MOVE    SPA-GMN-SYOSINYMD   TO  KT01-SYOSINYMD
           IF      SPA-GMN-LASTMID     =   SPACE
               IF      SPA-BEDFLG          =   ZERO
                   MOVE    "最終来院日"            TO  SPA-GMN-LASTMID
               ELSE
                   MOVE    "最終来院日（退院日）"  TO  SPA-GMN-LASTMID
               END-IF
           END-IF
           MOVE    SPA-GMN-LASTMID     TO  KT01-LASTMID
      *
      *    保険組合せ
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-HKN-MAX
               MOVE    SPA-GMN-HKN-LIST (IDX)  TO  KT01-HKNCOMBI-ITEM
                                                             (IDX)
           END-PERFORM
           MOVE    SPA-HKN-MAX         TO  KT01-HKNCOMBI-COUNT
      *
      *    診療科
           MOVE    SPA-GMN-SRYKA-G     TO  KT01-SRYKA
           PERFORM  VARYING   IDX     FROM    1   BY  1
                    UNTIL     IDX         >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKALST (IDX)  TO  KT01-SRYKA-ITEM
                                                             (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  KT01-SRYKA-COUNT
      *
      *    月診療日
           MOVE    ZERO                TO  IDY
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   31
               IF      IDX                 <=  SPA-SRYLST-DD
                   MOVE    IDX                 TO  WRK-Z9
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"                 TO  KANACHK-SYORI
                   MOVE    4                   TO  KANACHK-MAX-CNT
                   MOVE    WRK-Z9              TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-OUT-INPUT   TO  KT01-BDAY-LABEL (IDX)
               END-IF
               IF      SPA-NAI-RRKDAY (IDX)    =   ZERO
                   MOVE    "darkgray"   TO  KT01-BDAY-STYLE (IDX)
               ELSE
                   MOVE    "blue"          TO  KT01-BDAY-STYLE (IDX)
               END-IF
               IF      SPA-NAI-SRYDD       =   IDX
                   MOVE    "darkred"       TO  KT01-BDAY-STYLE (IDX)
               END-IF
      *!!
               IF      SPA-NAI-LSTDD       >=  IDX
                   MOVE    "black"         TO  KT01-BDAY-STYLE (IDX)
               END-IF
               IF     (IDX                 <=  SPA-SRYLST-DD )
                   MOVE    WIDGET-NORMAL       TO  KT01-BDAY-STATE(IDX)
      *            入院中
                   IF      SPA-K02-NYUDAY (IDX)    NOT =   ZERO
                       MOVE    WIDGET-INSENSITIVE
                                               TO  KT01-BDAY-STATE(IDX)
                   END-IF
               ELSE
                   MOVE    WIDGET-INSENSITIVE  TO  KT01-BDAY-STATE(IDX)
               END-IF
      *!!
           END-PERFORM
      *
      *    院内院外
           IF      SPA-INGAIKBN        =   "1"  OR  "0"
               CONTINUE
           ELSE
               MOVE    SPA-SYOHOKBN        TO  SPA-INGAIKBN
           END-IF
           EVALUATE    SPA-INGAIKBN
               WHEN    "1"
                   MOVE    "T"             TO  SPA-GMN-INGAI
                   MOVE    "T"             TO  KT01-INGAI-VALUE
                   MOVE    "院外"          TO  KT01-INGAI-LABEL
               WHEN    "0"
                   MOVE    "F"             TO  SPA-GMN-INGAI
                   MOVE    "F"             TO  KT01-INGAI-VALUE
                   MOVE    "院内"          TO  KT01-INGAI-LABEL
           END-EVALUATE
      *
      *    リハビリ・患者情報編集
           PERFORM 5003-TABBOHEN-SEC
      *    リハビリ発症日
      *    MOVE    ZERO               TO  IDY
      *    PERFORM  VARYING   IDX     FROM    1   BY  1
      *             UNTIL     IDX         >   7
      *        IF      SPA-GMN-RIHAYMD (IDX)  NOT =   SPACE
      *            ADD     1                      TO  IDY
      *            MOVE    SPA-GMN-RIHAYMD (IDX)  TO  KT01-TABOO-ITEM
      *                                                      (IDY)
      *        END-IF
      *    END-PERFORM
      *    急性発症日
      *    IF      SPA-GMN-KYUSEI     NOT =   SPACE
      *        ADD     1                  TO  IDY
      *        MOVE    SPA-GMN-KYUSEI     TO  KT01-TABOO-ITEM(IDY)
      *    END-IF
      *    禁忌
      *    PERFORM  VARYING   IDX     FROM    1   BY  1
      *             UNTIL    (IDX         >   SPA-TABOO-MAX )
      *                   OR (IDY         >=  40   )
      *        ADD     1                      TO  IDY
      *        MOVE    SPA-GMN-TABOO (IDX)    TO  KT01-TABOO-ITEM
      *                                                      (IDY)
      *****END-PERFORM
           MOVE    IDY                 TO  KT01-TABOO-COUNT
           MOVE    KT01-TABOO-ITEM(1)  TO  KT01-TABOO
      *
      *    施設入居中の特記事項を編集
           IF      SPA-PTTOKK-NAIYO    NOT =   SPACE
               MOVE    "blue"              TO  KT01-JIKANMSG-STYLE
               MOVE    SPA-PTTOKK-NAIYO    TO  KT01-JIKANMSG-VALUE
           END-IF
      *    公費期限切れ名称
           IF      SPA-GMN-KOHMSG      =   SPACE
               MOVE    SPACE               TO  KT01-KOHMSG-VALUE
           ELSE
               MOVE    "red"               TO  KT01-KOHMSG-STYLE
               MOVE    SPA-GMN-KOHMSG      TO  KT01-KOHMSG-VALUE
           END-IF
      *
           MOVE    SPA-GMN-DRCD-G      TO  KT01-DRNAME
      *    ドクター
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-DRCD-MAX
               MOVE    SPA-GMN-DRCDLST  (IDX)  TO  KT01-DRNAME-ITEM
                                                           (IDX)
           END-PERFORM
           MOVE    SPA-DRCD-MAX        TO  KT01-DRNAME-COUNT
      *    病名
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-BYOMEI-MAX
               MOVE    SPA-GMN-BYOMEI-NO(IDX)  TO  KT01-BYOMEIKA(IDX)
               MOVE    SPA-GMN-BYOMEI   (IDX)  TO  KT01-BYOMEI(IDX)
           END-PERFORM
           MOVE    SPA-BYOMEI-MAX      TO  KT01-BYOMEI-COUNT
      *
      *    他院紐付けあり
           IF      SPA-NAI-GRPKBN      =   1
               MOVE    "darkgreen"         TO  KT01-GRPMEI-STYLE
               MOVE    "【他】"            TO  KT01-GRPMEI-VALUE
           ELSE
               MOVE    "black"             TO  KT01-GRPMEI-STYLE
               MOVE    SPACE               TO  KT01-GRPMEI-VALUE
           END-IF
      *
      *H28.5
      *    処方名切替え
           IF     (SPA-GMN-PTNUM   NOT =   SPACE     )
            AND   (SPA-SRYYMD          >=  "20160401")
      *     院外投薬がある時
            AND   (FLG-INGAI-ARI       =   1         )
               IF      SPA-GENERICFLG      =   "1"
                   MOVE    SPACE               TO  KT01-MODE2-STYLE
                   MOVE    "(一般名)"          TO  KT01-MODE2-VALUE
               ELSE
                   MOVE    SPACE               TO  KT01-MODE2-STYLE
                   MOVE    "(銘柄名)"          TO  KT01-MODE2-VALUE
               END-IF
           END-IF
      *
      *H30.4
           IF     (SPA-SRYYMD          >=  "20180401")
               IF     (SPA-NINPUKBN-ARI    =   1    )
                   MOVE    "（妊婦）"      TO  KT01-MODE3-VALUE
                   MOVE    "blue"          TO  KT01-MODE3-STYLE
               ELSE
      *            手入力で妊婦
                   IF     (SPA-NINPUKBN-IN     =   1    )
                   MOVE    "（妊婦）"      TO  KT01-MODE3-VALUE
                   MOVE    "black"         TO  KT01-MODE3-STYLE
                   END-IF
               END-IF
           END-IF
      *
      *    医院名称
      ***  MOVE    SPA-HOSPNAME        TO  KT01-TITLE
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           IF      FLG-END             =   ZERO
      *         カーソルセット
                PERFORM 5001-MAPCUR-SEC
            END-IF
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
      *    排他中
           IF      SPA-LOCK            =   ZERO
               MOVE    WIDGET-NORMAL       TO  KT01-B12-STATE
               MOVE    WIDGET-NORMAL       TO  KT01-B01S-STATE
               MOVE    WIDGET-NORMAL       TO  KT01-B08S-STATE
               MOVE    WIDGET-NORMAL       TO  KT01-B09S-STATE
               MOVE    WIDGET-NORMAL       TO  KT01-B05S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  KT01-B12-STATE
               MOVE    WIDGET-INSENSITIVE  TO  KT01-B01S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  KT01-B08S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  KT01-B09S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  KT01-B05S-STATE
           END-IF
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面数量項目編集処理
      *****************************************************************
       5002-SURYO-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-ZAIKEI-H
           MOVE    SPACE               TO  WRK-KAISU-H 
           MOVE    SPACE               TO  WRK-KEI-H 
      *
      *    点数
           MOVE    SPA-GMN-TENSU(WRK-IDX)  TO  WRK-TENSU-X
      *    回数
           MOVE    ZERO                TO  IDK2
      *    外用薬の時、１
           IF     (SPA-COM-SRYKBN (WRK-IDX)    =   "23" )  AND
                  (SPA-GMN-KAISU  (WRK-IDX)    NOT =   SPACE )
               MOVE    1                   TO  WRK-KAISU-Z
           ELSE
               MOVE    SPA-GMN-KAISU  (WRK-IDX)    TO  WRK-KAISU-Z
           END-IF
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   4
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-ZAIKEI-H
           IF      SPA-GMN-KAISU (WRK-IDX) =   SPACE
               MOVE    SPACE               TO  WRK-KAKERU
           ELSE
               IF     (SPA-GMN-KEI   (WRK-IDX) =   ZERO )
      *            院外処方の時、回数表示
                   IF     (SPA-COM-SRYKBN(WRK-IDX)
                                               =   "21"  OR "22"
                                               OR  "23" )   OR
                          (SPA-COM-SRYSYUKBN(WRK-IDX)
                                               =   "148"  OR
                                                   "149"  )
                       MOVE    " X "               TO  WRK-KAKERU
      *ver.4.7
                       IF      SPA-COM-INGAITEN (WRK-IDX) >  ZERO
                           MOVE    SPACE           TO  WRK-TENSU-GX
                           MOVE    SPA-COM-INGAITEN(WRK-IDX)
                                                   TO  WRK-TENSU-GZ
                           MOVE    "("             TO  WRK-TENSU-G1
                           MOVE    ")"             TO  WRK-TENSU-G2
                           MOVE    WRK-TENSU-GX    TO  WRK-TENSU-X
                        END-IF
      *ver.4.7
                   ELSE
      *H26.10
      *                診療人数合計
                       IF      SPA-COM-INPUTCD (WRK-IDX)
                                                   =   "099140012"
                           MOVE    " X "               TO  WRK-KAKERU
                       ELSE
      *!! 
                       MOVE    SPACE               TO  WRK-KAISU-H
                       MOVE    SPACE               TO  WRK-KAKERU
      *!! 
                       END-IF
                   END-IF
               ELSE
                   MOVE    " X "               TO  WRK-KAKERU
               END-IF
           END-IF
           STRING  WRK-TENSU-X             DELIMITED   BY  SIZE
                   WRK-KAKERU              DELIMITED   BY  SIZE
                   WRK-KAISU-H             DELIMITED   BY  SPACE
                   INTO                    WRK-ZAIKEI-H
           END-STRING
      *
      *    画面編集
           MOVE    SPACE               TO  WRK-SURYO-HENSYU
           MOVE    SPA-GMN-SURYO   (WRK-IDX)   TO  WRK-H-SURYO
           MOVE    SPA-GMN-TANINAME(WRK-IDX)   TO  WRK-H-TANINAME
           MOVE    WRK-ZAIKEI-H                TO  WRK-H-ZAIKEI
           IF      SPA-GMN-KEI (WRK-IDX)   >=  ZERO
               MOVE    SPA-GMN-KEI (WRK-IDX)
                                           TO  WRK-KEI-Z
               MOVE    WRK-KEI-Z           TO  WRK-H-KEI
           ELSE
               MOVE    SPA-GMN-KEI (WRK-IDX)
                                           TO  WRK-KEI-Z2
               MOVE    WRK-KEI-Z2          TO  WRK-H-KEI
           END-IF
      *
           MOVE    WRK-SURYO-HENSYU    TO  KT01-SURYO (IDX)
           MOVE    "user-font"         TO  KT01-SURYO-STYLE(IDX)
      *@
      *    包括剤
           IF    ( SPA-COM-HOUKATU-ZAI (WRK-IDX) NOT =   ZERO)   OR
                 ((SPA-COM-HKNCOMBI(WRK-IDX)     =   "9999" )
              AND (SPA-HKNCOMBINUM           NOT =   "9999" ))
              MOVE    "blue"         TO  KT01-SURYO-STYLE(IDX)
           END-IF
           .
       5002-SURYO-HEN-EXT.
           EXIT.
      *TEST
      *H26.10
      *****************************************************************
      *    診療人数合計編集処理
      *****************************************************************
       50021-MEISYOHEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO-G
           IF     (SPA-GMN-INPUTCD(WRK-IDX + 1)(1:1)
                                       =   "*"         )
               MOVE    SPA-COM-NAME (WRK-IDX)  TO  WRK-MEISYO
               MOVE    WRK-MEISYO-G            TO  KT01-MEISYO(IDX)
               GO      TO      50021-MEISYOHEN-EXT
           END-IF
      *
           STRING  SPA-COM-NAME (WRK-IDX)  DELIMITED  BY  SPACE
                   "　　　【"              DELIMITED  BY  SIZE
                   SPA-SRYYMD (7:2)        DELIMITED  BY  SPACE
                   "日（"
                   WRK-KAISU-H             DELIMITED  BY  SPACE
                   "人）】"                DELIMITED  BY  SIZE
                   INTO                    WRK-MEISYO
           END-STRING
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MEISYO          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-MEISYO
      *
           MOVE    WRK-MEISYO-G        TO  KT01-MEISYO(IDX)
           .
       50021-MEISYOHEN-EXT.
           EXIT.
      *TEST
      *
      *
      *****************************************************************
      *    リハビリ・患者情報編集処理
      *****************************************************************
       5003-TABBOHEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX2    FROM    1   BY  1
     *****         UNTIL       IDX2    >   5
                   UNTIL       IDX2    >   6
               MOVE    ZERO                TO  IDY2
               EVALUATE    SPA-GMNHYOKBN (IDX2)
               WHEN    "1"
      *            リハビリ
                   MOVE    9              TO  IDY2
               WHEN    "2"
      *            禁忌
                   MOVE    1              TO  IDY2
               WHEN    "3"
      *            禁忌
                   MOVE    2              TO  IDY2
               WHEN    "4"
      *            禁忌
                   MOVE    3              TO  IDY2
               WHEN    "5"
      *            禁忌
                   MOVE    4              TO  IDY2
      *H28.10
               WHEN    "6"
      *            介護
                   MOVE    6              TO  IDY2
               END-EVALUATE
      *
               EVALUATE    IDY2
               WHEN    9
      *            リハビリ開始日
                   PERFORM  VARYING   IDX     FROM    1   BY  1
                            UNTIL    (IDX         >   7  )
                                 OR  (IDY         >=  40 )
                       IF      SPA-GMN-RIHAYMD (IDX)  NOT =   SPACE
                           ADD     1               TO  IDY
                           MOVE    SPA-GMN-RIHAYMD (IDX)
                                                   TO  KT01-TABOO-ITEM
                                                               (IDY)
                       END-IF
                   END-PERFORM
      *            急性発症日
                   IF     (SPA-GMN-KYUSEI     NOT =   SPACE )
                      AND (IDY                    <   40    )
                       ADD     1                  TO  IDY
                       MOVE    SPA-GMN-KYUSEI     TO  KT01-TABOO-ITEM
                                                              (IDY)
                   END-IF
               WHEN    1   THRU   4
      *            患者情報
                   PERFORM  VARYING   IDX     FROM    1   BY  1
                            UNTIL    (IDX         >   8  )
                                 OR  (IDY         >=  40 )
                       IF      SPA-GMN-TABOO (IDY2 IDX)
                                                   NOT =   SPACE
                           ADD     1               TO  IDY
                           MOVE    SPA-GMN-TABOO (IDY2 IDX)
                                                   TO  KT01-TABOO-ITEM
                                                               (IDY)
                       END-IF
                   END-PERFORM
      *H28.10
               WHEN    6
      *            介護情報
                   IF      (IDY                <   40 )
                      AND  (SPA-GMN-YOUKAIGO   NOT =   SPACE)
                       ADD     1               TO  IDY
                       MOVE    SPA-GMN-YOUKAIGO
                                                   TO  KT01-TABOO-ITEM
                                                               (IDY)
                   END-IF
               END-EVALUATE
           END-PERFORM
           .
       5003-TABBOHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソルセット
           MOVE    SPA-MAP-IDX         TO  WRK-IDX2
           IF      WRK-IDX2            =   ZERO
               MOVE    1               TO  WRK-IDX2
           END-IF
      *
           IF      SPA-GMN-CUR         =   1
               IF      WRK-IDX2            >   MAX-GMN
                   MOVE    10              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    患者番号が空白の時、患者番号へ
           IF      KT01-PTNUM       =  SPACE
               MOVE    00              TO  SPA-GMN-CUR
           END-IF
      *
      *    排他エラーの時、患者番号へ
           IF      SPA-ERRCD           =   "9999"
               MOVE    00              TO  SPA-GMN-CUR
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    10
                   MOVE    "B07"           TO  MCP-WIDGET
               WHEN    00
                   MOVE    "PTNUM"         TO  MCP-WIDGET
               WHEN    01
                   MOVE    "INPUTCD"       TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(8:2)
               WHEN    02
                   MOVE    "MEISYO"        TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(7:2)
               WHEN    03
                   MOVE    "KEI"           TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(4:2)
               WHEN    04
                   MOVE    "DOSELNUM"      TO  MCP-WIDGET
               WHEN    05
                   MOVE    "HKNCOMBI"      TO  MCP-WIDGET
               WHEN    06
                   MOVE    "SRYKA"         TO  MCP-WIDGET
               WHEN    07
                   MOVE    "SRYYM"         TO  MCP-WIDGET
               WHEN    08
                   MOVE    "SRYDD"         TO  MCP-WIDGET
               WHEN    09
                   MOVE    "DRNAME"        TO  MCP-WIDGET
               WHEN    90
      *            前回患者
                   MOVE    "B03"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           EVALUATE    SPA-ERRCD
               WHEN    "6001"
                   STRING  SPA-ERRMSG2     DELIMITED  BY  SPACE
                           "日は入院中の日です。"
                           "外来の入力はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
                   MOVE    SPACE               TO  SPA-ERRMSG2
               WHEN    "6002"
                   STRING  "この診療科・保険組合せで当月末まで"
                           "受診があります。まとめ入力はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "6003"
                   STRING  "複数科・複数保険組合での受診がある月です。"
                           "まとめ入力はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "6004"
                   STRING  "外来まとめ入力は平成２０年４月からの"
                           "入力です。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "6005"
                   STRING  "ドクターコードがありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "6010"
                   STRING  "保険組合せの期間外の日です。"
                           "診療日の変更はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "6011"
                   STRING  "削除できる受診がありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "6012"
                   STRING  "複数科・複数保険組合での受診があります。"
                           "外来まとめで削除はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "6013"
                   STRING  "診療科・保険組合で受診が月上限以上の算定が"
                           "あります。"
                           "外来まとめで削除はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "9009"
                   MOVE    "回数の／を入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "9010"
                   MOVE    "回数の／は１回のみです。"
                                       TO  SPA-ERRMSG
               WHEN    "6006"
                   MOVE    "入院中の患者です。"
                                       TO  SPA-ERRMSG
               WHEN    "6007"
                   STRING  SPA-NAI-LSTDD
                           "日まで受診があります。"
                           "まとめ入力できない日です。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "6008"
                   STRING  "この診療科・保険組合せで"
                           SPA-NAI-LSTDD
                           "日まで受診があります。"
                           "以降の日から入力して下さい。"
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "6015"
                   MOVE    "// で入力してください。"
                                               TO  SPA-ERRMSG
      *共通メッセージ
               WHEN    OTHER
                   MOVE    "KT01"              TO  LNK-SC96-PG
                   CALL    "ORCSC96"           USING    SPA-AREA
                                                        ORCSC96AREA
           END-EVALUATE
      *
           EVALUATE    SPA-ERRCD
               WHEN    "9999"
                   STRING  "他端末で使用中です。"
                           SLOCK-MSG           DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "9998"
                   STRING  "他端末で使用中です。"
                           "更新はできません。"
                           SLOCK-MSG           DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "9997"
                   STRING  "他端末で使用中です。"
                           SLOCK-MSG           DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
      *
           IF      SPA-ERRMSG2     NOT =   SPACE
      *        エラー画面２
               PERFORM 5101-KTERR2-SET-SEC
               GO      TO      510-ERRSET-EXT
           END-IF
      *
           MOVE    SPACE               TO  KTERR
           MOVE    SPA-ERRCD           TO  KTERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KTERR-ERRMSG
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "KT01"              TO  SPA-MOTOPG
           MOVE    "KTERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KTERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示２処理
      *****************************************************************
       5101-KTERR2-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  KTERR2
           INITIALIZE                      KTERR2
           MOVE    SPA-ERRCD           TO  KTERR2-ERRCODE
           MOVE    SPA-ERRMSG2         TO  KTERR2-ERRMSG(1:80)
           MOVE    SPA-ERRMSG          TO  KTERR2-ERRMSG2
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "KT01"              TO  SPA-MOTOPG
           MOVE    "KTERR2"            TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KTERR2"            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       5101-KTERR2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-KIDMSG
           EVALUATE    SPA-KIDCD
               WHEN    "3004"
                   STRING  "選択した診療科・保険組合せの当月診療内容を"
                           "すべて削除します。"
                           "復元はできません。"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0104"
                   MOVE    "処理を終了します。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    "0105"
                   STRING  "選択した診療科・保険組合せで"
                           SPA-NAI-LSTDD
                           "日まで受診があります。"
                           "入力中の内容を削除します。"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0123"
                   STRING  "選択した診療科・保険組合せで"
                           SPA-NAI-LSTDD
                           "日まで受診があります。"
                           "入力中の内容を削除します。"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0113"
                   STRING  "特定疾患処方管理加算が算定できます。"
                            "ＯＫで自動算定します。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0130"
                   STRING  "長期投薬加算が算定できます。"
                           "ＯＫで自動算定します。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *!!!!
               WHEN    "0213"
                   STRING  "特定疾患処方管理加算が算定できます。"
                            "ＯＫで自動算定します。"
                            "（併用算定警告該当有）"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0230"
                   STRING  "長期投薬加算が算定できます。"
                           "ＯＫで自動算定します。"
                            "（併用算定警告該当有）"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *!!!!
               WHEN    "0116"
                   STRING  "診療行為内容をクリアします。"
                           "よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0125"
                   STRING  "患者取消が選択されました。"
                           "表示内容をクリアします。よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0160"
                   STRING  "包括検査で同じ検査があります。"
                           "ＯＫですべての同一検査を削除します。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0190"
                   MOVE    "包括分入力に変更されました。"
                                       TO  WRK-KIDMSG
                   MOVE    "現在の診療内容をクリアしますか？"
                                       TO  WRK-KIDMSG(29:)
               WHEN    "2001"
                   STRING  "内服薬剤が７種類以上になります。"
                           "逓減しますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "2002"
                   STRING  "内服薬剤が７種類以上になります。"
                           "７種類以上の処方せん料にしますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0140"
                   STRING  "初回算定日を算定履歴に作成します。"
                                           DELIMITED  BY  SIZE
                           "取消はできません。よろしいですか？"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0150"
                   STRING  "包括算定エラーの項目があります。"
                                           DELIMITED  BY  SIZE
                           "エラー項目を削除します。"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0152"
                   STRING  "会計照会で最終受診日の変更がありました。"
                           "初期表示状態に変更します。"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *H22.4
               WHEN    "2003"
                   STRING  "手帳記載加算（薬剤情報提供料）を"
                           "算定します。"
                           "よろしいですか？"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *VER.4.6
               WHEN    "0138"
                   STRING   "診療科の保険組合せに変更しますか？"
                            "【"
                                       DELIMITED   BY  SIZE
                             HZN-SPA-GMN-HKNCOMBI-G
                                       DELIMITED   BY  "  "
                             "】"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *ver.4.7
               WHEN    "2004"
                   STRING  "用法まとめでは内服薬剤は７種類未満です。"
                           "７種類以上の処方せん料にしますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "2005"
                   STRING  "用法まとめでは内服薬剤が７種類以上に"
                           "なります。"
                           "７種類以上の処方せん料にしますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *ver.4.7
      *H26.10　改正
               WHEN    "2006"
                   STRING  "向精神薬多剤投与となります。"
                           "逓減しますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *H27.4　改正
               WHEN    "2007"
                   STRING  "３０日以上の投薬があります。"
                           "逓減しますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *!!
               WHEN    OTHER
                   MOVE    SPA-KIDCD
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KTID1
           INITIALIZE                      KTID1
           MOVE    SPA-KIDCD           TO  KTID1-ID1CODE
           MOVE    WRK-KIDMSG          TO  KTID1-ID1MSG
      *
           MOVE    "戻る"              TO  KTID1-B01
           MOVE    "ＯＫ"              TO  KTID1-B12
           EVALUATE    SPA-KIDCD
               WHEN    "0113"
               WHEN    "0130"
               WHEN    "0190"
               WHEN    "2001"
               WHEN    "2002"
      *
               WHEN    "0230"
               WHEN    "0213"
               WHEN    "2003"
               WHEN    "0138"
      *ver.4.7
               WHEN    "2004"
               WHEN    "2005"
               WHEN    "2006"
               WHEN    "2007"
                   MOVE    "ＮＯ"            TO  KTID1-B01
           END-EVALUATE
      *
           MOVE    "B12"               TO  MCP-WIDGET
           EVALUATE    SPA-KIDCD
               WHEN    "3004"
      *ver.4.7
               WHEN    "2004"
                   MOVE    "B01"               TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "KT01"              TO  SPA-MOTOPG
           MOVE    "KTID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KTID1"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-KTID2SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-KIDMSG
           EVALUATE    SPA-KTID2CD
      *
               WHEN    "3001"
                   STRING  "算定したい診察料のボタンを押下して下さい。"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-KIDCD
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KTID2
           INITIALIZE                      KTID2
           MOVE    SPA-KTID2CD         TO  KTID2-ID1CODE
           MOVE    WRK-KIDMSG          TO  KTID2-ID1MSG
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "KT01"              TO  SPA-MOTOPG
           MOVE    "KTID2"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KTID2"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KTID2SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ読み込み
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       940-SINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *980-PARA-READ-SEC         SECTION.
      *
      *    READ    PARA-FILE 
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *980-PARA-READ-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    患者病名読込
      *****************************************************************
       980-PTBYOMEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       980-PTBYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *ver.4.8
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       9100-SPATMP-KEYREAD-SEC        SECTION.
      *
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-SPATMP-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-SPATMP-READ-SEC
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-SPATMP-DBCLOSE-SEC
      *
           .
       9100-SPATMP-KEYREAD-EXT.
           EXIT.
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       990-SPATMP-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA2-REC    TO  SPATMP-REC
               MOVE    ZERO            TO  FLG-SPATMP
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           .
       990-SPATMP-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-SPATMP-DBSELECT-SEC                SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       910-SPATMP-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-SPATMP-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       990-SPATMP-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       910-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
      *
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       910-SPATMP-CALL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-GMN-PTID        TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
      *    排他中
           IF      SPA-LOCK            =   1
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    2                   TO  SPA-LOCK
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN2-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-PTID            TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
      *    排他中
           IF      SPA-LOCK            =   1
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    2                   TO  SPA-LOCK
           END-IF
           .
       990-LOCK-IN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    ワーク診療行為クリア
           MOVE    ZERO            TO  FLG-MOD-FLG
           PERFORM 4900-WKSRYACT-CLEAR-SEC
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
