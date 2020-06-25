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
       PROGRAM-ID.                 ORCSC91.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 算定歴チェック処理（診療行為入力のみＫ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01.06.05  警告メッセージ追加
      *  01.00.02    MCC-多々納   02.04.30  消炎鎮痛処置を同時算定へ追加
      *  01.00.03    MCC-多々納   02.05.17  労災用保険種別毎の追加
      *  01.00.04    NACL-多々納  02.08.06  同時算定チェックに継続管理加
      *                                     算追加
      *  01.00.05    NACL-多々納  02.12.17  入院時算定チェック変更
      *  01.00.06    NACL-多々納  03.03.17  介達牽引を同時算定へ追加
      *  01.00.07    NACL-多々納  04.09.02  労災の湿布処置算定修正
      *  01.00.08    NACL-多々納  05/11/22  MONFUNC 対応 他
      *  03.03.00    NACL-多々納  06/10/20  矯正固定等追加
      *  03.03.00    NACL-多々納  06/11/17  同時算定チェック方法修正
      *  03.04.00    NACL-多々納  06/12/05  上限回数ユーザ指定追加
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.01.00    NACL-多々納  07/11/XX  公害対応
      *  04.02.00    NACL-多々納  08/03/XX  平成２０年４月改正対応
      *                                     入院中１回のチェック追加
      *  04.05.00    NACL-多々納  10/03/XX  平成２２年４月改正対応
      *                                     他月数９９　対応
      *  04.06.00    NACL-多々納  10/12/XX  週回数対応
      *  04.07.00    NACL-多々納  11/11/28  同日再入院対応
      *                                     継続初回入院日対応
      *  04.07.00    NACL-多々納  13/02/XX  労災レセ電コード対応(H25.7)
      *  04.07.00    NACL-多々納  13/07/14  他月数　６０（５年）対応
      *  04.08.00    NACL-多々納  18/03/XX  平成３０年４月改正対応
      *  05.00.00    NACL-多々納  20/03/XX  令和０２年４月改正対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-NYU-CHK         PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
           03  FLG-TAISYO          PIC 9(01).
           03  FLG-SANTEI-ARI      PIC 9(01).
           03  FLG-TENSUPLUS-CHK   PIC 9(01).
      *
           03  FLG-SYOKAI-OK       PIC 9(01).
      *
           03  FLG-TBL-CHK          PIC 9(01).
           03  FLG-ERRKBN           PIC 9(01).
           03  FLG-DOUCHK           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX-Y2              PIC 9(02).
      *
           03  IDX-SAN             PIC 9(04).
           03  IDX-SAN2            PIC 9(04).
      *
           03  ID2                 PIC 9(02).
      *
           03  IDX-SN              PIC 9(04).
           03  IDD                 PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-ORC-DBPATH          PIC X(64).
      *
      *    算定回数チェック用
           03  WRK-STRYM.
               05  WRK-STRYY           PIC 9(04).
               05  WRK-STRMM           PIC 9(02).
           03  WRK-SANTEI-CNT          PIC 9(04).
           03  WRK-SANTEI-CNTW         PIC 9(04).
           03  WRK-CNT                 PIC 9(04).
           03  WRK-DAYCNT              PIC 9(04).
           03  WRK-DOCNT               PIC 9(04).
           03  WRK-DODAYCNT            PIC 9(03).
           03  WRK-CNT-OK              PIC 9(04).
           03  WRK-DAYCNT-SUM          PIC 9(04).
           03  WRK-DOCNT-SUM           PIC 9(04).
           03  WRK-DAYCNT-P            PIC 9(04).
           03  WRK-DOCNT-P             PIC 9(04).
           03  WRK-DCNT                PIC 9(04).
           03  WRK-JGNCNTETCM          PIC 9(02).
      *    他月数
           03  WRK-CHK-JGNCNTETCM      PIC 9(02).
      *
           03  WRK-SRYCD               PIC X(09).
           03  WRK-CODE                PIC X(03).
           03  WRK-CHK                 PIC X(01).
      *    現在件数
           03  WRK-SC91-CNT            PIC 9(04).
           03  WRK-SC91-DAYCNT         PIC 9(04).
           03  WRK-SC91-WEEKCNT        PIC 9(04).
      *    現在件数
           03  WRK-SC91-CHKCNT            PIC 9(04).
           03  WRK-SC91-DAYCHKCNT         PIC 9(04).
           03  WRK-SC91-WEEKCHKCNT        PIC 9(04).
      *
           03  WRK-TEI-CNT             PIC 9(04).
           03  WRK-ATO-CNT             PIC 9(04).
      *
           03  WRK-JGNCNTERR           PIC 9(01).
      *
           03  WRK-RIGAKU-RYO          PIC X(01). 
           03  WRK-RIGAKU-KBN          PIC X(01). 
      *    労災のリハビリ件数
           03  WRK-RSI-CNT             PIC 9(04).
           03  WRK-RSI-SRYCD           PIC X(09).
      *
           03  WRK-CHK-SRYCD           PIC X(09).
           03  WRK-CHK-SRYYMD.
               05  WRK-CHK-SRYYM           PIC X(06).
               05  WRK-CHK-SRYDD           PIC 9(02).
           03  WRK-CHK-NAME            PIC X(100).
           03  WRK-CHK-DD              PIC 9(02).
      *
      *    同時算定用
           03  WRK-CHK2-SRYCD           PIC X(09).
           03  WRK-CHK2-SRYYMD.
               05  WRK-CHK2-SRYYM           PIC X(06).
               05  WRK-CHK2-SRYDD           PIC 9(02).
           03  WRK-CHK2-NAME            PIC X(100).
      *
           03  WRK-DAYMAXCNT           PIC 9(04).
           03  WRK-DAY-YMD             PIC X(08).
      *    入院期間
           03  CNT-NYU                 PIC 9(03).
           03  WRK-NYU                 PIC 9(03).
           03  WRK-NYU2                PIC 9(03).
           03  WRK-YMD.
               05  WRK-YM              PIC X(06).
               05  WRK-DD              PIC 9(02).
           03  WRK-NYUINYMD            PIC X(08).
      *
      *    週
           03  WRK-YOBI                PIC 9(01).
           03  WRK-WEEK-CNT            PIC 9(04).
           03  WRK-STR-YMD.
               05  WRK-STR-YM          PIC 9(06).
               05  WRK-STR-DD          PIC 9(02).
           03  WRK-END-YMD.
               05  WRK-END-YM          PIC 9(06).
               05  WRK-END-DD          PIC 9(02).
           03  WRK-SRYYM               PIC X(06).
           03  WRK-SRYYMD              PIC X(08).
           03  IDX-Y3                  PIC 9(02).
           03  IDX-ST                  PIC 9(02).
           03  IDX-ED                  PIC 9(02).
           03  IDX-ED2                 PIC 9(02).
           03  WRK-WDOCNT              PIC 9(04).
      *
           03  WRK-CNT-DAY             PIC 9(02).
           03  WRK-CNT-MAX             PIC 9(03).
           03  WRK-CNT-ERR             PIC 9(03).
           03  WRK-CNT-MSG             PIC X(08).
      *
           03  WRK-HEN-HIZUKE.
               05  WRK-HEN-YY              PIC X(10).
               05  WRK-HEN-MMDD.
                   07  WRK-HEN-MM          PIC X(06).
                   07  WRK-HEN-DD          PIC X(06).
           03  WRK-HEN-DAY                 PIC X(22).
      *    数量編集
           03  WRK-HEN-ZZ              PIC ZZ9.
           03  WRK-HEN-CNT             PIC X(03).
           03  WRK-HEN-JCNT            PIC X(06).
           03  WRK-MOJI                PIC X(06).
           03  WRK-CNT-ERRJ            PIC X(06).
           03  WRK-CNT-MAXJ            PIC X(06).
           03  IDXM                    PIC 9(04).
      *
           03  WRK-ACT-DAY             PIC 9(03).
      *
           03  WRK-CHKCNT              PIC 9(03).
      *
           03  WRK-HZN-SRYCD           PIC X(09).
      *
      *    同時に算定する診療行為コード一覧
       01  TBL-DOUJI-AREA.
      *     寝たきり老人在宅総合診療科（処方箋交付あり）
      *    03  FILLER                  PIC X(04)   VALUE   "001".
      *    03  FILLER                  PIC X(09)   VALUE   "114701210".
      *     寝たきり老人在宅総合診療科（処方箋交付なし）
      *    03  FILLER                  PIC X(04)   VALUE   "001".
      *    03  FILLER                  PIC X(09)   VALUE   "114701710".
      *
      *     薬剤情報提供料
           03  FILLER                  PIC X(04)   VALUE   "0020".
           03  FILLER                  PIC X(09)   VALUE   "120002370".
      *     薬剤情報提供料（老人手帳記載なし）
           03  FILLER                  PIC X(04)   VALUE   "0021".
           03  FILLER                  PIC X(09)   VALUE   "113701410".
      *     薬剤情報提供料（老人手帳記載あり）
           03  FILLER                  PIC X(04)   VALUE   "0021".
           03  FILLER                  PIC X(09)   VALUE   "113701310".
      *
      *     特定疾患処方管理加算処方箋料
           03  FILLER                  PIC X(04)   VALUE   "003".
           03  FILLER                  PIC X(09)   VALUE   "120002570".
      *     特定疾患処方管理加算処方料
           03  FILLER                  PIC X(04)   VALUE   "003".
           03  FILLER                  PIC X(09)   VALUE   "120002270".
      *
      *    小児科外来診療科（院外処方）初診
           03  FILLER                  PIC X(04)   VALUE   "004".
           03  FILLER                  PIC X(09)   VALUE   "113003510".
      *    小児科外来診療科（院内処方）初診
           03  FILLER                  PIC X(04)   VALUE   "004".
           03  FILLER                  PIC X(09)   VALUE   "113003710".
      *    小児科外来診療科（院外処方）再診
           03  FILLER                  PIC X(04)   VALUE   "004".
           03  FILLER                  PIC X(09)   VALUE   "113003610".
      *    小児科外来診療科（院内処方）再診
           03  FILLER                  PIC X(04)   VALUE   "004".
           03  FILLER                  PIC X(09)   VALUE   "113003810".
      *H28.4
      *    小児かかりつけ診療科（院内処方）初診
           03  FILLER                  PIC X(04)   VALUE   "004".
           03  FILLER                  PIC X(09)   VALUE   "113019710".
      *    小児かかりつけ診療科（院内処方）再診
           03  FILLER                  PIC X(04)   VALUE   "004".
           03  FILLER                  PIC X(09)   VALUE   "113019810".
      *    小児かかりつけ診療科（院外処方）初診
           03  FILLER                  PIC X(04)   VALUE   "004".
           03  FILLER                  PIC X(09)   VALUE   "113019910".
      *    小児かかりつけ診療科（院外処方）再診
           03  FILLER                  PIC X(04)   VALUE   "004".
           03  FILLER                  PIC X(09)   VALUE   "113020010".
      *
      *    消炎鎮痛処置テーブル
           03  FILLER                  PIC X(04)   VALUE   "005".
           03  FILLER                  PIC X(09)   VALUE   "140029610".
           03  FILLER                  PIC X(04)   VALUE   "005".
           03  FILLER                  PIC X(09)   VALUE   "140040310".
      *    湿布処置
           03  FILLER                  PIC X(04)   VALUE   "005P".
           03  FILLER                  PIC X(09)   VALUE   "140002210".
           03  FILLER                  PIC X(04)   VALUE   "005P".
           03  FILLER                  PIC X(09)   VALUE   "140002310".
      *    介達牽引
           03  FILLER                  PIC X(04)   VALUE   "005".
           03  FILLER                  PIC X(09)   VALUE   "140048010".
      *H18/04から
      *    介達牽引（矯正固定）
           03  FILLER                  PIC X(04)   VALUE   "005H".
           03  FILLER                  PIC X(09)   VALUE   "140030350".
      *    介達牽引（変形機械矯正術）
           03  FILLER                  PIC X(04)   VALUE   "005H".
           03  FILLER                  PIC X(09)   VALUE   "140048250".
      *    腰部固定帯固定
           03  FILLER                  PIC X(04)   VALUE   "005H".
           03  FILLER                  PIC X(09)   VALUE   "140048350".
      *    胸部固定帯固定
           03  FILLER                  PIC X(04)   VALUE   "005H".
           03  FILLER                  PIC X(09)   VALUE   "140048450".
      *    低出力レーザ照射
           03  FILLER                  PIC X(04)   VALUE   "005H".
           03  FILLER                  PIC X(09)   VALUE   "140048550".
      *    肛門処置
           03  FILLER                  PIC X(04)   VALUE   "005H".
           03  FILLER                  PIC X(09)   VALUE   "140002450".
      *まで
      *Ｈ２０．４から
      *    人工腎臓（その他）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140007710".
      *    人工腎臓（４時間未満）（H30.3　まで）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140036710".
      *    人工腎臓（４時間以上から５時間未満）（H30.3　まで）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140051010".
      *    人工腎臓（５時間以上）（H30.3　まで）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140051110".
      *    持続緩徐式血液濾過
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140029850".
      *    人工腎臓（慢性維持透析濾過）（H30.3　まで）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140052810".
      *
      *Ｈ３０．４　人工腎臓の追加
      *    人工腎臓（慢性維持透析１（４時間未満）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140057810".
      *    人工腎臓（慢性維持透析１（４時間以上５時間未満）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140057910".
      *    人工腎臓（慢性維持透析１（５時間以上）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140058010".
      *    人工腎臓（慢性維持透析２（４時間未満）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140058110".
      *    人工腎臓（慢性維持透析２（４時間以上５時間未満）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140058210".
      *    人工腎臓（慢性維持透析２（５時間以上）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140058310".
      *    人工腎臓（慢性維持透析３（４時間未満）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140058410".
      *    人工腎臓（慢性維持透析３（４時間以上５時間未満）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140058510".
      *    人工腎臓（慢性維持透析３（５時間以上）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140058610".
      *    人工腎臓（慢性維持透析１（４時間未満）（経過措置）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140059310".
      *    人工腎臓（慢性維持透析１（４時間以上５時間未満）（経過措置）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140059410".
      *    人工腎臓（慢性維持透析１（５時間以上）（経過措置）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140059510".
      *
      *Ｒ０２．４　人工腎臓の追加
      *    人工腎臓（慢性維持透析１（４時間未満）（イを除く）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140060210".
      *    人工腎臓（慢性維持透析１（４時間以上５時間未満）（ロを除く）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140060310".
      *    人工腎臓（慢性維持透析１）（５時間以上）（ハを除く）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140060410".
      *    人工腎臓（慢性維持透析２）（４時間未満）（イを除く）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140060510".
      *    人工腎臓（慢性維持透析２）（４時間以上５時間未満）（ロを除く）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140060610".
      *    人工腎臓（慢性維持透析２）（５時間以上）（ハを除く）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140060710".
      *    人工腎臓（慢性維持透析３）（４時間未満）（イを除く）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140060810".
      *    人工腎臓（慢性維持透析３）（４時間以上５時間未満）（ロを除く）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140060910".
      *    人工腎臓（慢性維持透析３）（５時間以上）（ハを除く）
           03  FILLER                  PIC X(04)   VALUE   "008".
           03  FILLER                  PIC X(09)   VALUE   "140061010".
      *
      *
      *    継続管理加算（一般）
           03  FILLER                  PIC X(04)   VALUE   "006".
           03  FILLER                  PIC X(09)   VALUE   "112007170".
      *    継続管理加算（老人）
           03  FILLER                  PIC X(04)   VALUE   "006".
           03  FILLER                  PIC X(09)   VALUE   "112702270".
      *
      *    長期投薬加算（処方箋料）
           03  FILLER                  PIC X(04)   VALUE   "007".
           03  FILLER                  PIC X(09)   VALUE   "120003270".
      *    長期投薬加算（処方料）
           03  FILLER                  PIC X(04)   VALUE   "007".
           03  FILLER                  PIC X(09)   VALUE   "120003170".
      *
       01  TBL-DOUJI-AREA-R            REDEFINES  TBL-DOUJI-AREA.
      *****03  TBL-DOUJI-OCC           OCCURS   9   INDEXED   TBL-IDX.
      *****03  TBL-DOUJI-OCC           OCCURS   13  INDEXED   TBL-IDX.
      **** 03  TBL-DOUJI-OCC           OCCURS   24  INDEXED   TBL-IDX.
      *****03  TBL-DOUJI-OCC           OCCURS   34  INDEXED   TBL-IDX.
      *H30.4
      *    03  TBL-DOUJI-OCC           OCCURS   46  INDEXED   TBL-IDX.
           03  TBL-DOUJI-OCC           OCCURS   55  INDEXED   TBL-IDX.
               05  TBL-CODE                PIC X(03).
               05  TBL-CHK                 PIC X(01).
               05  TBL-SRYCD               PIC X(09).
      *01  TBL-MAX                     PIC 9(04)   VALUE   9.
      *01  TBL-MAX                     PIC 9(04)   VALUE   18.
      *01  TBL-MAX                     PIC 9(04)   VALUE   24.
      *01  TBL-MAX                     PIC 9(04)   VALUE   34.
      *01  TBL-MAX                     PIC 9(04)   VALUE   46.
       01  TBL-MAX                     PIC 9(04)   VALUE   55.
      *
      *    保険別算定履歴テーブル
           COPY   "CMSANTEITBL.INC".
      *    リハビリ同一算定コードテーブル
           COPY   "CMRIHACDTBL.INC".
      *H25.7
      *    労災コード置換えテーブル 
           COPY   "CMRSICNGTBL.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *    算定履歴
       01  HZN-SANTEI-AREA.
           02  HZN-SANTEI-REC          OCCURS  30.
           COPY    "CPSANTEI.INC"      REPLACING
                                       //SANTEI-// BY //HZN-SANTEI-//.
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    ワーク点数マスタ−
           COPY    "CPTENSUWK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC91.INC".
      *
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC91AREA
           SPA-SCR-REC
           SPA-AREA 
           TNS-TENSU-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPACE               TO  LNK-SC91-ERRCD
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *    入院回数処理なし
           IF      LNK-SC91-SRYCD(1:1) =   "*"  OR  SPACE
               GO      TO      000-PROC-EXT
           END-IF
      *
           IF      LNK-SC91-KBN        =   "2"  OR  "3"  OR  "4"
               MOVE    LNK-SC91-SRYCD      TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   1
                   MOVE    "0001"              TO  LNK-SC91-ERRCD
               END-IF
           END-IF
      *
      *    リハビリチェック（H18.4)
           IF     (LNK-SC91-SRYCD(1:1) =   "1" )  AND
                  (TNS-SRYKBN          =   "80" )
               SET     TBL-RIH             TO  1
               SEARCH  TBL-RIHABIRI-OCC    VARYING   TBL-RIH
                   AT  END
                       MOVE    ZERO            TO  FLG-RIHA-OK
                   WHEN    LNK-SC91-SRYCD      =   TBL-RIHA-SRYCD
                                                           (TBL-RIH)
                       MOVE    1               TO  FLG-RIHA-OK
               END-SEARCH 
      *        Ｈ２０．４から対応
      *        労災固有のリハビリテーション料
               IF     (SPA-HKNKBN          =   1   )  AND
                      (SPA-SRYYMD          >=  "20080401"  )
                   IF     (LNK-SC91-SRYCD(1:3)     =   "101" )
                      AND (TNS-SRYKBN              =   "80"  )
                      AND (TNS-SRYSYUKBN           =   "800" )
                      AND (TNS-DATAKBN             =   "1"   )
                      AND (TNS-CDKBN-KBN           =   "H"   )
                       MOVE    1                   TO  FLG-RIHA-OK
                   END-IF
               END-IF
      *        H22.4 から　新規のリハビリ料コードは区分で判定
               IF     (SPA-SRYYMD              >=  "20100401")
                  AND (FLG-RIHA-OK             =   ZERO    )
                  AND (TNS-CDKBN-KBN           =   "H"     )
                  AND (TNS-CDKBN-KBNNUM        =   "001"
                                               OR  "002"   )
                  AND (TNS-CDKBN-KBNNUM-EDA    =   "00"    )
                   MOVE    1               TO  FLG-RIHA-OK
               END-IF
      *    H28.4 廃用症候群
               IF     (SPA-SRYYMD              >=  "20160401")
                  AND (FLG-RIHA-OK             =   ZERO    )
                  AND (TNS-CDKBN-KBN           =   "H"     )
                  AND (TNS-CDKBN-KBNNUM        =   "001"   )
                  AND (TNS-CDKBN-KBNNUM-EDA    =   "02"    )
                   MOVE    1               TO  FLG-RIHA-OK
               END-IF
      *
               IF      FLG-RIHA-OK         =   1
                   MOVE    "099800201"         TO  LNK-SC91-SRYCD
                   MOVE    LNK-SC91-SRYCD      TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF      FLG-TENSU           =   1
                       MOVE    "0001"              TO  LNK-SC91-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    K02,K02NYUの場合のみとする（薬剤情報提供料チェックの為）
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           IF     (LNK-SC91-ERRCD      =   SPACE )
               IF     (LNK-SC91-KBN        =   "8"
                                           OR  "9" )
      *            入院期間
                   PERFORM 2001-NYUIN-CHEK-SEC
               ELSE
                   IF      TNS-SANTEIRRKKBN    =   ZERO
                       CONTINUE
                   ELSE
      *            算定履歴チェック
                       PERFORM 100-MAIN-CHEKI-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *
      *****************************************************************
      *    算定回数チェック処理
      *****************************************************************
       100-MAIN-CHEKI-SEC      SECTION.
      *
      *    訂正の時、前回算定時の算定回数
           IF      LNK-SC91-SYORIFLG   =   1
               MOVE    LNK-SC91-SRYCD      TO  WRK-SRYCD
               PERFORM 1001-TEISEI-CNT-SEC
           ELSE
               MOVE    ZERO            TO  WRK-TEI-CNT
           END-IF
      *    １日上限で単位が分以外の時、数量
           IF      (LNK-SC91-SURYO     >   1    )
               AND (TNS-JGNCNT1D       >   ZERO )
               AND (TNS-TANICD     NOT =   "001")
      *H24.8
      *        ３桁以上は、999とする
               IF     (LNK-SC91-KAISU  *   LNK-SC91-SURYO)
                                       >   999
                   MOVE    999             TO  LNK-SC91-KAISU
               ELSE
      *H24.8
               COMPUTE LNK-SC91-KAISU      =   LNK-SC91-KAISU
                                           *   LNK-SC91-SURYO
               END-IF
           END-IF
      *
           EVALUATE    LNK-SC91-KBN
               WHEN    "1"
               WHEN    "2"
               WHEN    "3"
               WHEN    "4"
      *            同時算定コード存在のチェック
                   PERFORM 1001-CODECHK-SEC
      *            算定歴　回数チェック
                   PERFORM 100-SANTEI-CHK-SEC
                   IF     (LNK-SC91-ERRCD  =   SPACE ) AND
                          (WRK-CODE    NOT =   SPACE )
      *                同時算定歴チェック
                       PERFORM 200-DOUJI-CHK-SEC
                   END-IF
           END-EVALUATE
      *
           .
       100-MAIN-CHEKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回算定時の算定回数処理
      *****************************************************************
       1001-TEISEI-CNT-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-TEI-CNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE )  OR
                              (LNK-SC91-TEI-SRYCD(IDX)
                                           =   SPACE )
               IF      WRK-SRYCD           =   LNK-SC91-TEI-SRYCD(IDX)
                   ADD     LNK-SC91-TEI-KAISU(IDX)
                                           TO  WRK-TEI-CNT
               END-IF
           END-PERFORM
           .
       1001-TEISEI-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険毎のコードの判定処理
      *****************************************************************
       1000-SANTEI-CHK-SEC      SECTION.
      *
           SET     TBL-SAN             TO  1
           SEARCH  TBL-SANTEI-OCC      VARYING   TBL-SAN
               AT  END
                   MOVE    ZERO            TO  FLG-HKNSAN
                   MOVE    SPACE           TO  WRK-RIGAKU-RYO
                   MOVE    SPACE           TO  WRK-RIGAKU-KBN
                   MOVE    SPACE           TO  WRK-RSI-KBN
               WHEN    WRK-SRYCD       =   TBL-SANTEI-SRYCD
                                                          (TBL-SAN)
                   MOVE    1               TO  FLG-HKNSAN
                   MOVE    TBL-RIGAKU-RYO (TBL-SAN)
                                           TO  WRK-RIGAKU-RYO
                   MOVE    TBL-RIGAKU-KBN (TBL-SAN)
                                           TO  WRK-RIGAKU-KBN
                   MOVE    TBL-RSI-KBN    (TBL-SAN)
                                           TO  WRK-RSI-KBN
           END-SEARCH
      *    Ｈ１８．１０から対応
           IF      WRK-RIGAKU-RYO      =   "H"
               IF      SPA-SRYYMD      >=  "20061001"
                   MOVE    SPACE           TO  WRK-RIGAKU-RYO
               ELSE
                   MOVE    ZERO            TO  FLG-HKNSAN
                   MOVE    SPACE           TO  WRK-RIGAKU-RYO
                   MOVE    SPACE           TO  WRK-RIGAKU-KBN
                   MOVE    SPACE           TO  WRK-RSI-KBN
               END-IF
           END-IF
      *H25.7
      *    労災保険コード変更
           IF     (WRK-SRYCD(1:3)      =   "101"      )  AND
                  (SPA-SRYYMD          >=  "20130701" )
               PERFORM 10001-RSICD-SANTEI-CHK-SEC
           END-IF
      *
      *    Ｈ２２年４月から追加のコードは固定で判定
      *     (COPY に追加するのは、パッケージ対応で）
           IF      WRK-SRYCD           =   "099800181"
                                       OR  "099800182"
      *H28.4
             OR  (WRK-SRYCD(1:7)       =   "0998002"   )
      *        早期・初期リハ開始日・終了日
      *        がん患者リハ開始日・終了日
               MOVE    1               TO  FLG-HKNSAN
               MOVE    SPACE           TO  WRK-RIGAKU-RYO
               MOVE    SPACE           TO  WRK-RIGAKU-KBN
               MOVE    SPACE           TO  WRK-RSI-KBN
           END-IF
      *
           IF      WRK-RIGAKU-RYO      =   SPACE
               MOVE    ZERO                TO  FLG-HKN-SANTEI
           ELSE
               MOVE    1                   TO  FLG-HKN-SANTEI
           END-IF
      *
           IF      WRK-RSI-KBN         =   "R"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-RSI-KBN
           END-IF
      *
           .
       1000-SANTEI-CHK-EXT.
           EXIT.
      *H25.7
      *****************************************************************
      *    労災コード保険毎のコードの判定処理
      *****************************************************************
       10001-RSICD-SANTEI-CHK-SEC      SECTION.
      *
      *    労災のコードの判定
           SET     TBL-RCN             TO  1
           SEARCH  TBL-RSICHG-OCC      VARYING   TBL-RCN
               AT  END
                   MOVE    SPACE           TO  WRK-RSI-KBN
               WHEN    WRK-SRYCD           =   TBL-RSICHG-ATO-SRYCD
                                                          (TBL-RCN)
                   MOVE    TBL-RSICHG-KBN (TBL-RCN)
                                           TO  WRK-RSI-KBN
           END-SEARCH
      *
           IF      WRK-RSI-KBN         =   "R"
               MOVE    1               TO  FLG-HKNSAN
           ELSE
               IF      WRK-RSI-KBN         =   "A"
                   MOVE    1               TO  FLG-HKNSAN
               ELSE
                   MOVE    ZERO            TO  FLG-HKNSAN
               END-IF
           END-IF
           .
       10001-RSICD-SANTEI-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    算定回数チェック処理
      *****************************************************************
       100-SANTEI-CHK-SEC      SECTION.
      *
           MOVE    LNK-SC91-SRYCD      TO  WRK-SRYCD
           MOVE    ZERO                TO  WRK-CNT
           MOVE    ZERO                TO  WRK-DOCNT
           MOVE    ZERO                TO  WRK-DAYCNT
           MOVE    ZERO                TO  WRK-ATO-CNT
           INITIALIZE                      WRK-CHK-SRYYMD
           MOVE    ZERO                TO  WRK-CHK-DD
           MOVE    SPACE               TO  WRK-DAY-YMD
      *
           IF      LNK-SC91-KBN        =   "4"
               MOVE    ZERO            TO  WRK-DOCNT
           ELSE
      *        今回入力分カウント
               PERFORM 1003-INPUT-CHK-SEC
           END-IF
      *
      *    上限他月数
           IF      TNS-JGNCNTETCM     >   ZERO
               PERFORM 1002-SANTEI-ETC-SEC
               IF      LNK-SC91-KT01FLG    =   "1"
      *            外来まとめ月数
                   PERFORM 10029-KT01SANTEI-ETC-SEC
               END-IF
           ELSE
               IF      LNK-SC91-KT01FLG    =   "1"
      *            外来まとめ月数
                   PERFORM 1009-KT01SANTEI-SEC
               ELSE
                   PERFORM 1001-SANTEI-YMD-SEC
               END-IF
               MOVE    TNS-JGNCNT          TO  LNK-SC91-MAXCNT
               MOVE    ZERO                TO  WRK-JGNCNTETCM
           END-IF
           MOVE    TNS-JGNCNT1D        TO  LNK-SC91-DAYMAXCNT
      *D   MOVE    TNS-JGNCNT          TO  LNK-SC91-MAXCNT
      *
      *    週
           MOVE    TNS-JGNCNT1W        TO  LNK-SC91-WEEKMAXCNT
           IF      TNS-JGNCNT1W        >   ZERO
               PERFORM 10099-JGNCNT1W-CHK-SEC
           END-IF
      *
           MOVE    TNS-JGNCNTERR       TO  WRK-JGNCNTERR
      *
           COMPUTE LNK-SC91-CNT        =   LNK-SC91-CNT
                                       +   WRK-DOCNT
           COMPUTE LNK-SC91-DAYCNT     =   LNK-SC91-DAYCNT
                                       +   WRK-DODAYCNT
      *
      *    入力回数が１回以上の時、回数に加算
           IF     (LNK-SC91-KBN        =   "1"  OR  "3" )  AND
                  (LNK-SC91-KAISU      >   1    )
               COMPUTE WRK-SC91-CNT        =   LNK-SC91-CNT
                                           +  (LNK-SC91-KAISU
                                           -   1   )
               COMPUTE WRK-SC91-DAYCNT     =   LNK-SC91-DAYCNT
                                           +  (LNK-SC91-KAISU
                                           -   1   )
               COMPUTE WRK-SC91-WEEKCNT    =   LNK-SC91-WEEKCNT
                                           +  (LNK-SC91-KAISU
                                           -   1   )
           ELSE
               MOVE    LNK-SC91-CNT        TO  WRK-SC91-CNT
               MOVE    LNK-SC91-DAYCNT     TO  WRK-SC91-DAYCNT
               MOVE    LNK-SC91-WEEKCNT    TO  WRK-SC91-WEEKCNT
           END-IF
      *
           MOVE    WRK-SC91-CNT        TO  WRK-SC91-CHKCNT
           MOVE    WRK-SC91-DAYCNT     TO  WRK-SC91-DAYCHKCNT
           MOVE    WRK-SC91-WEEKCNT    TO  WRK-SC91-WEEKCHKCNT
      *
      *    上限回数チェック・エラー編集
           MOVE    TNS-JGNCNTERR       TO  WRK-JGNCNTERR
           MOVE    ZERO                TO  FLG-DOUCHK
           PERFORM 1009-CNTCHK-SYORI-SEC
      *
      *    訂正後の回数
           MOVE    WRK-ATO-CNT         TO  LNK-SC91-ATO-CNT
      *
           MOVE    TNS-JGNCNTERR       TO  WRK-JGNCNTERR
           .
       100-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    上限回数チェック・エラー編集処理
      *****************************************************************
       1009-CNTCHK-SYORI-SEC      SECTION.
      *
           MOVE    ZERO               TO  FLG-ERRKBN
      *    当月上限
           IF     (LNK-SC91-MAXCNT     >   ZERO   )  AND
                  (LNK-SC91-MAXCNT     <=  WRK-SC91-CHKCNT  )
               IF      FLG-DOUCHK          =   ZERO
                   MOVE    "0091"              TO  LNK-SC91-ERRCD
               ELSE
                   MOVE    "0033"              TO  LNK-SC91-ERRCD
               END-IF
               MOVE    1                   TO  FLG-ERRKBN
               MOVE    LNK-SC91-MAXCNT     TO  WRK-CNT-MAX
               MOVE    WRK-SC91-CHKCNT     TO  WRK-CNT-ERR
               MOVE    "月上限："          TO  WRK-CNT-MSG
               IF     (WRK-CHK-DD          >   ZERO  )   AND
                      (WRK-CHK-DD          >   WRK-CHK-SRYDD)
                   MOVE    SPA-SRYYMD(1:6)     TO  WRK-CHK-SRYYM
                   MOVE    WRK-CHK-DD          TO  WRK-CHK-SRYDD
               END-IF
      *        上限回数エラー処理＝９の時、回数エラーは警告とする
               IF      (WRK-CNT        =   ZERO)
      *H25.1
                OR     (FLG-DOUCHK     =   1   )
                   IF       WRK-JGNCNTERR      =   9
                       MOVE    "K004"              TO  LNK-SC91-ERRCD
                   END-IF
      *            人工腎臓の時メッセージ変更
                   IF      WRK-CODE            =   "008"
                       MOVE    "K242"          TO  LNK-SC91-ERRCD
                   END-IF
               END-IF
           END-IF
      *    薬剤情報提供料はチェック判定
           IF      WRK-CODE            =   "002"
               IF      SPA-SRYYMD          <   "20100401"
                   IF      (SPA-YAKJYOCHK-FLG   =   "0"   ) OR
                          ((WRK-CHK             =   "0")  AND
                           (SPA-YAKJYO-1        =   2  ))   OR
                          ((WRK-CHK             =   "1" )  AND
                          ((SPA-YAKJYO-2        =   2 ) OR
                           (SPA-YAKJYO-3        =   2 )) )
                       MOVE    LNK-SC91-ERRCD      TO  LNK-SC91-ERRCD2
                       MOVE    SPACE               TO  LNK-SC91-ERRCD
                   END-IF
               ELSE
      *        H22.4　から薬情のコードは１つ
                   IF      (SPA-YAKJYOCHK-FLG   =   "0"   ) OR
                         (( WRK-CHK             =   "0" ) AND
                          ((SPA-YAKJYO-1        =   2  ) OR
                           (SPA-YAKJYO-2        =   2  ) OR
                           (SPA-YAKJYO-3        =   2  )) )
                       MOVE    LNK-SC91-ERRCD      TO  LNK-SC91-ERRCD2
                       MOVE    SPACE               TO  LNK-SC91-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    労災で消炎鎮痛処置の時、当日上限回数を変更する
           IF     (SPA-HKNKBN          =   1  )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
               EVALUATE    WRK-RIGAKU-KBN
      *            マッサージ
                   WHEN    "M"
      *            器具
                   WHEN    "K"
                       MOVE    3               TO  LNK-SC91-DAYMAXCNT
      *            介達牽引
                   WHEN    "T"
                       MOVE    3               TO  LNK-SC91-DAYMAXCNT
      *            湿布
                   WHEN    "P"
                       MOVE    ZERO            TO  LNK-SC91-DAYMAXCNT
      *            腰部固定帯固定等
                   WHEN    "E"
                       MOVE    3               TO  LNK-SC91-DAYMAXCNT
               END-EVALUATE
           END-IF
      *    当日上限
           IF     (LNK-SC91-DAYMAXCNT  >   ZERO   )   AND
                  (LNK-SC91-DAYMAXCNT  <=  WRK-SC91-DAYCHKCNT )
               IF      FLG-DOUCHK          =   ZERO
      ****         MOVE    "0091"              TO  LNK-SC91-ERRCD
                   MOVE    "0902"              TO  LNK-SC91-ERRCD
               ELSE
                   MOVE    "0033"              TO  LNK-SC91-ERRCD
      *            消炎鎮痛処置の時、エラーメッセージを換える
                   IF      WRK-CODE            =   "005"
                       MOVE    "1022"          TO  LNK-SC91-ERRCD
                   END-IF
               END-IF
               MOVE    WRK-DAY-YMD         TO  WRK-CHK-SRYYMD
               MOVE    2                   TO  FLG-ERRKBN
               MOVE    LNK-SC91-DAYMAXCNT  TO  WRK-CNT-MAX
               MOVE    WRK-SC91-DAYCHKCNT  TO  WRK-CNT-ERR
               MOVE    "日上限："          TO  WRK-CNT-MSG
      *        上限回数エラー処理＝９の時、回数エラーは警告とする
               IF      (WRK-DAYCNT     =   ZERO)
      *H25.1
                OR     (FLG-DOUCHK     =   1   )
                   IF       TNS-JGNCNTERR      =   9
      **               MOVE    "K091"              TO  LNK-SC91-ERRCD
                       MOVE    "K902"              TO  LNK-SC91-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    週上限
           IF     (LNK-SC91-WEEKMAXCNT     >   ZERO   )   AND
                  (LNK-SC91-WEEKMAXCNT     <=  WRK-SC91-WEEKCHKCNT )
               MOVE    "0901"              TO  LNK-SC91-ERRCD
               MOVE    3                   TO  FLG-ERRKBN
               MOVE    LNK-SC91-WEEKMAXCNT TO  WRK-CNT-MAX
               MOVE    WRK-SC91-WEEKCHKCNT TO  WRK-CNT-ERR
               MOVE    "週上限："          TO  WRK-CNT-MSG
      *        上限回数エラー処理＝９の時、回数エラーは警告とする
               IF       WRK-CNT            =   ZERO
                   IF       TNS-JGNCNTERR      =   9
                       MOVE    "K903"              TO  LNK-SC91-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    労災毎の算定時
           IF      FLG-DOUCHK          =   ZERO
               IF     (LNK-SC91-ERRCD  NOT =   SPACE  )  AND
                      (WRK-RSI-KBN     NOT =   SPACE  )
                   MOVE    "0094"          TO  LNK-SC91-ERRCD
      *H25.10
      *            上限回数エラー処理＝９の時、警告とする
                   IF       TNS-JGNCNTERR      =   9
                       MOVE    "K094"          TO  LNK-SC91-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *    月上限のエラーの時、算定日を編集
           IF     (LNK-SC91-ERRCD  NOT =   SPACE  )
             AND  (WRK-CHK-SRYYM   NOT =   SPACE  )
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-CHK-SRYYMD      TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
      *
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HEN-HIZUKE
               MOVE    SPACE               TO  WRK-HEN-DAY
               IF      WRK-CHK-SRYYM(1:4)  =   SPA-SRYYMD(1:4)
                   MOVE    WRK-HEN-MMDD        TO  WRK-HEN-DAY
               ELSE
                   MOVE    WRK-HEN-HIZUKE      TO  WRK-HEN-DAY
               END-IF
      *        回数全角変換
               MOVE    WRK-CNT-ERR         TO  WRK-HEN-ZZ
               PERFORM 800-SURYO-HEN-SEC
               MOVE    WRK-HEN-JCNT        TO  WRK-CNT-ERRJ
      *
               MOVE    WRK-CNT-MAX         TO  WRK-HEN-ZZ
               PERFORM 800-SURYO-HEN-SEC
               MOVE    WRK-HEN-JCNT        TO  WRK-CNT-MAXJ
      *
               MOVE    SPACE               TO  LNK-SC91-ERRMSG
               EVALUATE    FLG-ERRKBN
               WHEN    2
      *            日上限
                   IF      WRK-CNT-ERR         >   1
                        STRING WRK-HEN-DAY         DELIMITED  BY  SPACE
                               "に"                DELIMITED  BY  SIZE
                               WRK-CNT-ERRJ        DELIMITED  BY  SPACE
                               "回算定済です。"    DELIMITED  BY  SIZE
                               INTO                LNK-SC91-ERRMSG
                       END-STRING
                   ELSE
                       STRING WRK-HEN-DAY          DELIMITED  BY  SPACE
                              "算定済みです。"     DELIMITED  BY  SIZE
                              INTO                LNK-SC91-ERRMSG
                       END-STRING
                   END-IF
               WHEN    1
      *            月上限
                   IF      WRK-CNT-ERR         =   WRK-CNT-MAX
                       IF      WRK-CNT-MAX         =   1
                           STRING
                               WRK-HEN-DAY         DELIMITED  BY  SPACE
                               "に"                DELIMITED  BY  SIZE
                               WRK-CHK-NAME        DELIMITED  BY  SPACE
                               WRK-CNT-ERRJ        DELIMITED  BY  SPACE
                               "回算定済です。"    DELIMITED  BY  SIZE
                               INTO                LNK-SC91-ERRMSG
                           END-STRING
                       ELSE
                           STRING
                               WRK-HEN-DAY         DELIMITED  BY  SPACE
                               "で"                DELIMITED  BY  SIZE
                               WRK-CHK-NAME        DELIMITED  BY  SPACE
                               WRK-CNT-ERRJ        DELIMITED  BY  SPACE
                               "回算定済です。"    DELIMITED  BY  SIZE
                               INTO                LNK-SC91-ERRMSG
                           END-STRING
                       END-IF
                   ELSE
                       STRING  WRK-HEN-DAY         DELIMITED  BY  SPACE
                               "で"                DELIMITED  BY  SIZE
                               WRK-CHK-NAME        DELIMITED  BY  SPACE
                               WRK-CNT-ERRJ        DELIMITED  BY  SPACE
                               "回算定済です。"    DELIMITED  BY  SIZE
                               "【"
                               WRK-CNT-MSG
                               WRK-CNT-MAXJ        DELIMITED  BY  SPACE
                               "回】"              DELIMITED  BY  SIZE
                               INTO                LNK-SC91-ERRMSG
                       END-STRING
                   END-IF
      *            他月
                   IF      WRK-JGNCNTETCM      >   ZERO   AND
                                               <   99
                       MOVE    WRK-JGNCNTETCM         TO  WRK-HEN-ZZ
                       PERFORM 800-SURYO-HEN-SEC
                       MOVE    SPACE               TO  LNK-SC91-ERRMSG
                       STRING  WRK-HEN-DAY         DELIMITED  BY  SPACE
                               "で"                DELIMITED  BY  SIZE
                               WRK-CHK-NAME        DELIMITED  BY  SPACE
                               WRK-CNT-ERRJ        DELIMITED  BY  SPACE
                               "回算定済です。"    DELIMITED  BY  SIZE
                               "【"
                               WRK-HEN-JCNT
                               "カ月：" 
                               WRK-CNT-MAXJ        DELIMITED  BY  SPACE
                               "回】"              DELIMITED  BY  SIZE
                               INTO                LNK-SC91-ERRMSG
                       END-STRING
                   END-IF
                   IF      WRK-JGNCNTETCM      =   99
                       MOVE    SPACE               TO  LNK-SC91-ERRMSG
                       STRING  WRK-HEN-DAY         DELIMITED  BY  SPACE
                               "で"                DELIMITED  BY  SIZE
                               WRK-CHK-NAME        DELIMITED  BY  SPACE
                               WRK-CNT-ERRJ        DELIMITED  BY  SPACE
                               "回算定済です。"    DELIMITED  BY  SIZE
                               INTO                LNK-SC91-ERRMSG
                       END-STRING
                   END-IF
               WHEN    3
      *            週上限
                   IF      WRK-CNT-ERR         =   WRK-CNT-MAX
                       IF      WRK-CNT-MAX         =   1
                           STRING
                               WRK-HEN-DAY         DELIMITED  BY  SPACE
                               "に"                DELIMITED  BY  SIZE
                               WRK-CHK-NAME        DELIMITED  BY  SPACE
                               WRK-CNT-ERRJ        DELIMITED  BY  SPACE
                               "回算定済です。"    DELIMITED  BY  SIZE
                               INTO                LNK-SC91-ERRMSG
                           END-STRING
                       ELSE
                           STRING
                               WRK-HEN-DAY         DELIMITED  BY  SPACE
                               "で"                DELIMITED  BY  SIZE
                               WRK-CHK-NAME        DELIMITED  BY  SPACE
                               WRK-CNT-ERRJ        DELIMITED  BY  SPACE
                               "回算定済です。"    DELIMITED  BY  SIZE
                               INTO                LNK-SC91-ERRMSG
                           END-STRING
                       END-IF
                   ELSE
                       STRING  WRK-HEN-DAY         DELIMITED  BY  SPACE
                               "で"                DELIMITED  BY  SIZE
                               WRK-CHK-NAME        DELIMITED  BY  SPACE
                               WRK-CNT-ERRJ        DELIMITED  BY  SPACE
                               "回算定済です。"    DELIMITED  BY  SIZE
                               "【"
                               WRK-CNT-MSG
                               WRK-CNT-MAXJ        DELIMITED  BY  SPACE
                               "回】"              DELIMITED  BY  SIZE
                               INTO                LNK-SC91-ERRMSG
                       END-STRING
                   END-IF
               END-EVALUATE
           END-IF
           IF     (LNK-SC91-ERRCD  NOT =   SPACE  )
             AND  (LNK-SC91-ERRMSG     =   SPACE  )
               IF      WRK-CHK-NAME        =   SPACE
                    MOVE    "今回算定済み"      TO  LNK-SC91-ERRMSG
               ELSE
                   STRING  "今回　"            DELIMITED  BY  SIZE
                           WRK-CHK-NAME        DELIMITED  BY  SPACE
                           "算定済み"
                           INTO                LNK-SC91-ERRMSG
                   END-STRING
               END-IF
           END-IF
           .
       1009-CNTCHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数チェック処理（診療年月）
      *****************************************************************
       1001-SANTEI-YMD-SEC      SECTION.
      *
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
           END-EVALUATE
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-HKNKBN      NOT =   ZERO)
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           IF      WRK-RSI-KBN         =   SPACE
      *        主キー
               MOVE    "key"               TO  WRK-ORC-DBPATH
           ELSE
      *        労災毎の算定
               MOVE    "key3"              TO  WRK-ORC-DBPATH
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SANTEI          =   1
               INITIALIZE              SANTEI-REC
           END-IF
      *H25.7
      *    労災コード変更前コードでの検索
           IF     (WRK-SRYCD(1:3)      =   "101"      )
             AND  (SPA-SRYYMD          >=  "20130701" )
             AND  (WRK-RSI-KBN         =   "R"        )
             AND  (FLG-SANTEI          =    1         )
               IF    (SPA-COB-TEKSTYMD    <   "20130701" )
                   PERFORM 10011-RSICD-SANTEI-YMD-SEC
               ELSE
      *            労災の初診時ブラッシング料（処置）(手術)の時
                   IF      WRK-SRYCD           =   "101400030"
                                               OR  "101500150"
                       PERFORM 10011-RSICD-SANTEI-YMD-SEC
                   END-IF
               END-IF
           END-IF
      *
      *    算定歴の回数をチェックする
           IF      TNS-JGNCNT          >   ZERO
               MOVE    TNS-JGNCNT          TO  LNK-SC91-MAXCNT
               MOVE    SANTEI-MSANTEICNT   TO  LNK-SC91-CNT
           ELSE
               MOVE    SANTEI-MSANTEICNT   TO  LNK-SC91-CNT
           END-IF
      *    労災でリハビリの時３月以内の件数はカウントしない
      *### 公害は分からないのでカウントする
      **** IF     (SPA-HKNKBN      NOT =   ZERO  )  AND
           IF     (SPA-HKNKBN          =   1     )  AND
                  (FLG-HKN-SANTEI      =   1     )
               MOVE    LNK-SC91-CNT        TO  WRK-RSI-CNT
               PERFORM 10012-RSI-MCNT-SEC
               MOVE    WRK-RSI-CNT         TO  LNK-SC91-CNT
           END-IF
      *    エラー内容用
           IF      LNK-SC91-CNT        >   ZERO
               MOVE    SANTEI-SRYYM        TO  WRK-CHK-SRYYM
               MOVE    SANTEI-MSANTEID     TO  WRK-CHK-SRYDD
           END-IF
      *
      *    上限１日
      *****IF     (TNS-JGNCNT1D        >   ZERO  )  AND
           IF     (SPA-SRYYMD(7:2)     NUMERIC   )  AND
                  (SPA-SRYYMD(7:2)     >   ZERO  )
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               MOVE    TNS-JGNCNT1D        TO  LNK-SC91-DAYMAXCNT
               MOVE    SANTEI-DAY (IDX-Y2) TO  LNK-SC91-DAYCNT
               IF      SANTEI-DAY (IDX-Y2)     >   ZERO
                   MOVE    SPA-SRYYMD          TO  WRK-DAY-YMD
               END-IF
           END-IF
      *
      *    訂正の時、訂正分はカウントしない
           IF      LNK-SC91-SYORIFLG   =   1
               IF      LNK-SC91-DAYCNT     >=  WRK-TEI-CNT
                   COMPUTE LNK-SC91-DAYCNT     =   LNK-SC91-DAYCNT
                                               -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO                TO  LNK-SC91-DAYCNT
               END-IF
               IF      LNK-SC91-CNT        >=  WRK-TEI-CNT
                   COMPUTE LNK-SC91-CNT        =   LNK-SC91-CNT
                                               -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO                TO  LNK-SC91-CNT
               END-IF
      *        上限１日の時、訂正日以降の回数を求める
      *****    IF      TNS-JGNCNT1D        >   ZERO
                   MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                   ADD     1                   TO  IDX-Y2
                   PERFORM VARYING     IDY     FROM    IDX-Y2  BY  1
                           UNTIL       IDY     >   31
                       ADD     SANTEI-DAY (IDY)    TO  WRK-ATO-CNT
                   END-PERFORM
      *****    END-IF
           END-IF
      *    月上限最終算定日
           IF     (LNK-SC91-CNT        >   ZERO )
            AND   (LNK-SC91-MAXCNT     >   ZERO )
               IF      LNK-SC91-SYORIFLG   =   1
      *            訂正分は対象外
                   MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               ELSE
                   MOVE    ZERO                TO  IDX-Y2
               END-IF
               MOVE    ZERO                TO  WRK-CHKCNT
               PERFORM VARYING     IDD     FROM    31  BY -1
                       UNTIL      (IDD     <   1   )
                   IF     (IDD             =   IDX-Y2 )
                      AND (WRK-TEI-CNT     >   ZERO   )
                       IF      SANTEI-DAY (IDD) >=  WRK-TEI-CNT
                           COMPUTE WRK-CHKCNT  =   (SANTEI-DAY (IDD)
                                               -   WRK-TEI-CNT )
                       ELSE
                           MOVE    ZERO            TO  WRK-CHKCNT
                       END-IF
                   ELSE
                       MOVE    SANTEI-DAY (IDD)    TO  WRK-CHKCNT
                   END-IF
                   IF      WRK-CHKCNT      >   ZERO
                       MOVE    IDD                 TO  WRK-CHK-SRYDD
                       MOVE    1                   TO  IDD
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       1001-SANTEI-YMD-EXT.
           EXIT.
      *****************************************************************
      *    労災コード変更前コードでの検索処理
      *****************************************************************
       10011-RSICD-SANTEI-YMD-SEC      SECTION.
      *
      *    変更前の労災コード判定
           SET     TBL-RCN             TO  1
           SEARCH  TBL-RSICHG-OCC      VARYING   TBL-RCN
               AT  END
                   MOVE    SPACE           TO  WRK-RSI-SRYCD
               WHEN    WRK-SRYCD           =   TBL-RSICHG-ATO-SRYCD
                                                          (TBL-RCN)
                   MOVE    TBL-RSICHG-MAE-SRYCD (TBL-RCN)
                                           TO  WRK-RSI-SRYCD
           END-SEARCH
      *    労災の初診時ブラッシング料（処置）の時、手術を検索
           IF      WRK-SRYCD           =   "101400030"
               MOVE    "101500150"         TO  WRK-RSI-SRYCD
           END-IF
      *
           IF      WRK-RSI-SRYCD   NOT =   SPACE
      *        算定履歴検索
               INITIALIZE                  SANTEI-REC
               MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
               MOVE    SPA-PTID            TO  SANTEI-PTID
               MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
               MOVE    WRK-RSI-SRYCD       TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
      *
      *        労災毎の算定
               MOVE    "key3"              TO  WRK-ORC-DBPATH
      *
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SANTEI
                   INITIALIZE                      SANTEI-REC
               END-IF
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *
           IF      WRK-RSI-SRYCD   NOT =   SPACE
               IF     (LNK-SC91-SYORIFLG   =   1 )
                  AND (WRK-TEI-CNT         =   ZERO )
                   MOVE    WRK-SRYCD           TO  WRK-HZN-SRYCD
                   MOVE    WRK-RSI-SRYCD       TO  WRK-SRYCD
                   PERFORM 1001-TEISEI-CNT-SEC
                   MOVE    WRK-HZN-SRYCD       TO  WRK-SRYCD
               END-IF
           END-IF
      *
           .
       10011-RSICD-SANTEI-YMD-EXT.
           EXIT.
      *****************************************************************
      *    週数処理
      *****************************************************************
       10099-JGNCNT1W-CHK-SEC      SECTION.
      *
           INITIALIZE                      WRK-CHK-SRYYMD
      *    曜日指定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY3-AREA
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    SPA-SRYYMD          TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY3-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY3-YOBI       TO  WRK-YOBI
           ELSE
               MOVE    1                   TO  WRK-YOBI
           END-IF
      *
           IF      WRK-YOBI            =   7
      *        日曜
               MOVE    SPA-SRYYMD          TO  WRK-STR-YMD
           ELSE
      *        診療日の日曜日
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-SRYYMD          TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               COMPUTE LNK-DAY6-ZOGEN      =   WRK-YOBI   *   -1
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  WRK-STR-YMD
           END-IF
      *    土曜日
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-STR-YMD         TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    6                   TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-END-YMD
      *
      *    算定履歴検索処理
           MOVE    WRK-STR-YM          TO  WRK-SRYYM
           PERFORM 1091-SANTEIREC-KEN-SEC
      *
      *    MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           MOVE    WRK-END-YMD         TO  WRK-SRYYMD
      *
           MOVE    ZERO                TO  WRK-WEEK-CNT
           MOVE    WRK-STR-DD          TO  IDX-ST
           IF      WRK-STR-YMD(1:6)    =   WRK-SRYYMD(1:6)
               MOVE    WRK-SRYYMD(7:2)     TO  IDX-ED
           ELSE
               MOVE    31                  TO  IDX-ED
           END-IF
           MOVE    ZERO                TO  WRK-CNT-DAY
      *    外来まとめの当月分は対象外
           IF     (LNK-SC91-KT01FLG    =   "1"             )
              AND (WRK-SRYYM           =   WRK-SRYYMD(1:6) )
               MOVE    32                  TO  IDX-ST
           END-IF
      *
           PERFORM VARYING     IDD     FROM    IDX-ST  BY  1
                   UNTIL       IDD     >   IDX-ED
               IF     (SANTEI-DAY (IDD)    >   ZERO    )
                   ADD     SANTEI-DAY (IDD)    TO  WRK-WEEK-CNT
                   IF     (WRK-TEI-CNT         >   ZERO  )  AND
                          (IDD                 =   WRK-SRYYMD(7:2))
                       CONTINUE
                   ELSE
                       MOVE    IDD                 TO  WRK-CNT-DAY
                   END-IF
               END-IF
           END-PERFORM
           IF     (FLG-SANTEI          =   ZERO  )  AND
                  (WRK-CNT-DAY         >   ZERO  )
               MOVE    SANTEI-SRYYM        TO  WRK-CHK-SRYYM
               MOVE    WRK-CNT-DAY         TO  WRK-CHK-SRYDD
           END-IF
      *
           IF      WRK-STR-YMD(1:6)    NOT =   WRK-SRYYMD(1:6)
      *    外来まとめの当月分は対象外
           AND    (LNK-SC91-KT01FLG    NOT =   "1"             )
               MOVE    WRK-SRYYMD (1:6)    TO  WRK-SRYYM
               PERFORM 1091-SANTEIREC-KEN-SEC
               MOVE    1                   TO  IDX-ST
               MOVE    WRK-SRYYMD(7:2)     TO  IDX-ED
               MOVE    ZERO                TO  WRK-CNT-DAY
               PERFORM VARYING     IDD     FROM    IDX-ST  BY  1
                       UNTIL       IDD     >   IDX-ED
                   IF     (SANTEI-DAY (IDD)    >   ZERO    )
                       ADD     SANTEI-DAY (IDD)    TO  WRK-WEEK-CNT
                       IF     (WRK-TEI-CNT         >   ZERO  )  AND
                              (IDD                 =   WRK-SRYYMD(7:2))
                           CONTINUE
                       ELSE
                           MOVE    IDD                 TO  WRK-CNT-DAY
                       END-IF
                   END-IF
               END-PERFORM
               IF     (FLG-SANTEI          =   ZERO  )  AND
                      (WRK-CNT-DAY         >   ZERO  )
                   MOVE    SANTEI-SRYYM        TO  WRK-CHK-SRYYM
                   MOVE    WRK-CNT-DAY         TO  WRK-CHK-SRYDD
               END-IF
           END-IF
      *
      *    訂正の時、訂正分はカウントしない
           IF      LNK-SC91-SYORIFLG   =   1
               IF      WRK-WEEK-CNT        >=   WRK-TEI-CNT
                   COMPUTE WRK-WEEK-CNT        =   WRK-WEEK-CNT
                                               -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO                TO  WRK-WEEK-CNT
               END-IF
           END-IF
      *
      *今月分のみチェック
      *    日曜＝診療月
           IF      WRK-STR-YMD(1:6)    =   SPA-SRYYMD(1:6)
               MOVE    WRK-STR-DD          TO  IDX-ST
           ELSE
               MOVE    1                   TO  IDX-ST
           END-IF
      *    土曜＝診療月
           IF      WRK-END-YMD(1:6)    =   SPA-SRYYMD(1:6)
               MOVE    WRK-END-DD          TO  IDX-ED
           ELSE
               MOVE    31                  TO  IDX-ED
           END-IF
      *
      *    入院今回対象以外入力分カウント
           IF      SPA-NYUGAIKBN       =   "1"
               PERFORM 100992-INPUT-CNTCHK-SEC
           END-IF
      *
           IF      LNK-SC91-KT01FLG    =   "1"
      *        外来まとめ月数
               PERFORM 10099-KT01SANTEI-WEEK-SEC
           END-IF
      *
      *    算定歴の回数をチェックする
           MOVE    WRK-WEEK-CNT        TO  LNK-SC91-WEEKCNT
      *
           .
       10099-JGNCNT1W-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院今回対象以外入力分カウント処理
      *****************************************************************
       100992-INPUT-CNTCHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-WDOCNT
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
           MOVE    ZERO                TO  WRK-CNT-DAY
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
      *
               IF      (SPA-COM-HKNCOMBI(IDX)  NOT =   "9999")
                  AND ((SPA-COM-INPUTCD (IDX)
                                           =   LNK-SC91-SRYCD  )  OR
                       (SPA-COM-CHK-SRYCD (IDX)
                                           =   LNK-SC91-SRYCD  ))
      *            入院で算定日以外の集計
                   PERFORM VARYING     IDD     FROM    IDX-ST   BY  1
                           UNTIL      (IDD     >   IDX-ED    )
                       IF      SPA-COM-NYUINDAY(IDX IDD)     >   ZERO
                           IF     (LNK-SC91-KBN    =   "1"  OR  "3")
                           AND    (IDX             =   LNK-SC91-GMN-IDX)
                           AND    (IDD             >=  IDX-Y2      )
                               CONTINUE
                           ELSE
                               ADD     SPA-COM-KAISU(IDX)
                                                       TO  WRK-WDOCNT
                               MOVE    IDD             TO  WRK-CNT-DAY
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           COMPUTE WRK-WEEK-CNT        =   WRK-WEEK-CNT
                                       +   WRK-WDOCNT
           IF      WRK-WDOCNT          >   ZERO
               IF      WRK-CNT-DAY         >   WRK-CHK-SRYDD
                   MOVE    SPA-SRYYMD(1:6)     TO  WRK-CHK-SRYYM
                   MOVE    WRK-CNT-DAY         TO  WRK-CHK-SRYDD
               END-IF
           END-IF
           .
       100992-INPUT-CNTCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来まとめ週処理
      *****************************************************************
       10099-KT01SANTEI-WEEK-SEC      SECTION.
      *
      *    保険毎のコードクリア
           MOVE    SPACE           TO  WRK-RSI-KBN
      *    外来まとめ算定履歴編集処理
           PERFORM 10091-KT01SANTEI-HEN-SEC
      *
      **   MOVE    ZERO                TO  WRK-WEEK-CNT
           MOVE    ZERO                TO  WRK-CNT-DAY
           PERFORM VARYING     IDD     FROM    1  BY  1
                   UNTIL       IDD     >   IDX-ED
               IF     (IDD                 >=  IDX-ST  )
                AND   (SANTEI-DAY (IDD)    >   ZERO    )
                   ADD     SANTEI-DAY (IDD)    TO  WRK-WEEK-CNT
                   MOVE    IDD                 TO  WRK-CNT-DAY
               END-IF
           END-PERFORM
           IF     (WRK-CNT-DAY         >   ZERO  )
               MOVE    SANTEI-SRYYM        TO  WRK-CHK-SRYYM
               MOVE    WRK-CNT-DAY         TO  WRK-CHK-SRYDD
           END-IF
           .
       10099-KT01SANTEI-WEEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴検索処理
      *****************************************************************
       1091-SANTEIREC-KEN-SEC      SECTION.
      *
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYYM           TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           EVALUATE    TNS-SANTEIRRKKBN
               WHEN    1
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    2
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO                TO  SANTEI-SRYKA
               WHEN    3
                   MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
               WHEN    4
                   MOVE    SPA-NYUGAIKBN       TO  SANTEI-NYUGAIKBN
                   MOVE    SPA-SRYKA           TO  SANTEI-SRYKA
           END-EVALUATE
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-HKNKBN      NOT =   ZERO)
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           IF      WRK-RSI-KBN         =   SPACE
      *        主キー
               MOVE    "key"               TO  WRK-ORC-DBPATH
           ELSE
      *        労災毎の算定
               MOVE    "key3"              TO  WRK-ORC-DBPATH
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SANTEI          =   1
               INITIALIZE              SANTEI-REC
           END-IF
      *
           .
       1091-SANTEIREC-KEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来まとめ月数処理
      *****************************************************************
       1009-KT01SANTEI-SEC      SECTION.
      *
      *    保険毎のコードクリア
           MOVE    SPACE           TO  WRK-RSI-KBN
      *    外来まとめ算定履歴編集処理
           PERFORM 10091-KT01SANTEI-HEN-SEC
      *
      *    算定歴の回数をチェックする
           IF      TNS-JGNCNT          >   ZERO
               MOVE    TNS-JGNCNT          TO  LNK-SC91-MAXCNT
               MOVE    SANTEI-MSANTEICNT   TO  LNK-SC91-CNT
           ELSE
               MOVE    SANTEI-MSANTEICNT   TO  LNK-SC91-CNT
           END-IF
      *    上限１日
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
           MOVE    TNS-JGNCNT1D        TO  LNK-SC91-DAYMAXCNT
           MOVE    SANTEI-DAY (IDX-Y2) TO  LNK-SC91-DAYCNT
      *    エラー内容用
           IF      LNK-SC91-CNT        >   ZERO
               MOVE    SANTEI-SRYYM        TO  WRK-CHK-SRYYM
               MOVE    SANTEI-MSANTEID     TO  WRK-CHK-SRYDD
           END-IF
           .
       1009-KT01SANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来まとめ算定履歴編集処理
      *****************************************************************
       10091-KT01SANTEI-HEN-SEC      SECTION.
      *
           INITIALIZE                  SANTEI-REC
           PERFORM VARYING     IDX-SN      FROM    1   BY  1
                   UNTIL      (IDX-SN      >   100 )   OR
                              (LNK-SC91-KT01-SRYCD  (IDX-SN) =   SPACE)
               IF      WRK-SRYCD       =   LNK-SC91-KT01-SRYCD (IDX-SN)
                   INITIALIZE                  SANTEI-REC
                   MOVE    SPA-SRYYMD(1:6)     TO  SANTEI-SRYYM
      *            MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                   MOVE    31                  TO  IDX-Y2
                   PERFORM VARYING     IDD         FROM    1   BY  1
                           UNTIL      (IDD         >   IDX-Y2 )
                       MOVE    LNK-SC91-KT01-DAY (IDX-SN IDD)
                                                   TO  SANTEI-DAY (IDD)
                       ADD     SANTEI-DAY (IDD)    TO  SANTEI-MSANTEICNT
                       IF      SANTEI-DAY (IDD)    >   ZERO
                           IF      SANTEI-MSANTEID     =   SPACE
                               MOVE    IDD         TO  SANTEI-MSANTEID
                           END-IF
                       END-IF
                   END-PERFORM
                   MOVE    100             TO  IDX-SN
               END-IF
           END-PERFORM
           .
       10091-KT01SANTEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災でリハビリの時３月以降の件数カウント処理（月数回数）
      *****************************************************************
       10012-RSI-MCNT-SEC      SECTION.
      *
      *    月上限チェックなし
           IF      SPA-SRYYMD          >=  "20100401"
               GO      TO      10012-RSI-MCNT-EXT
           END-IF
      *
      *    システム月＜傷病日＋３月
           IF       SPA-SRYYMD(1:6)    <   SPA-RSI-SHOBYOYMD3(1:6)
               MOVE    ZERO            TO  WRK-RSI-CNT
           ELSE
      *        システム月＝傷病日＋３月
               IF       SPA-SRYYMD(1:6)    =   SPA-RSI-SHOBYOYMD3(1:6)
                   MOVE    SPA-RSI-SHOBYOYMD3(7:2)     TO  IDX-Y2
                   MOVE    ZERO                TO  WRK-RSI-CNT
                   PERFORM VARYING     IDY     FROM    IDX-Y2  BY  1
                           UNTIL       IDY     >   31
                       ADD     SANTEI-DAY (IDY)    TO  WRK-RSI-CNT
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       10012-RSI-MCNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数チェック処理（月数回数）
      *****************************************************************
       1002-SANTEI-ETC-SEC      SECTION.
      *
      *    算定開始年月
           MOVE    SPA-SRYYMD(1:6)     TO  WRK-STRYM
      *
           MOVE    TNS-JGNCNTETCM      TO  WRK-CHK-JGNCNTETCM
      *
           IF      TNS-JGNCNTETCM      =   99
      *        上限他月数が９９の時は、全履歴１回とする
               MOVE    ZERO                TO  WRK-STRYM
           ELSE
      *
      *        最長５年とする
               IF      WRK-CHK-JGNCNTETCM  >   60
                   MOVE    60                  TO  WRK-CHK-JGNCNTETCM
               END-IF
      *
               PERFORM UNTIL   WRK-CHK-JGNCNTETCM  <=  12
                   COMPUTE WRK-STRYY       =   WRK-STRYY    -   1
                   COMPUTE WRK-CHK-JGNCNTETCM  =   WRK-CHK-JGNCNTETCM
                                       -   12
               END-PERFORM
      *
               IF      WRK-STRMM           <   WRK-CHK-JGNCNTETCM
                   COMPUTE WRK-STRYY       =   WRK-STRYY    -   1
                   COMPUTE WRK-STRMM       =   WRK-STRMM
                                           +   12
                                       -  (WRK-CHK-JGNCNTETCM   -  1 )
               ELSE
                   COMPUTE WRK-STRMM       =   WRK-STRMM
                                       -  (WRK-CHK-JGNCNTETCM   -  1 )
               END-IF
           END-IF
      *
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-HKNKBN      NOT =   ZERO)
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
               INITIALIZE                  SANTEI-REC
           END-IF
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
           PERFORM UNTIL   FLG-SANTEI      =   1
               IF      SANTEI-SRYYM    <   WRK-STRYM
                   MOVE    1               TO  FLG-SANTEI
               ELSE
      *!!!
      *        外来まとめは当月分をふくめない
               IF     (LNK-SC91-KT01FLG    =   "1"   )
               AND    (SANTEI-SRYYM        =   SPA-SRYYMD(1:6))
                   CONTINUE
               ELSE
      *!!!
                   IF      WRK-CHK-SRYYM   =   SPACE
                       MOVE    SANTEI-SRYYM        TO  WRK-CHK-SRYYM
                       MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                       ADD     1                   TO  IDX-Y2
                       PERFORM VARYING     IDX-Y2  FROM    31  BY  -1
                               UNTIL       IDX-Y2  <   1
                           IF      SANTEI-DAY (IDX-Y2)    >   ZERO
                               MOVE    IDX-Y2      TO  WRK-CHK-SRYDD
                               MOVE    1           TO  IDX-Y2
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE    SANTEI-MSANTEICNT   TO  WRK-SANTEI-CNTW
      *            労災でリハビリの時３月以内の件数はカウントしない
      *### 公害は分からないのでカウントする
      *!!!!        IF     (SPA-HKNKBN      NOT =   ZERO  )  AND
                   IF     (SPA-HKNKBN          =   1     )  AND
                          (FLG-HKN-SANTEI      =   1     )
                       MOVE    WRK-SANTEI-CNTW     TO  WRK-RSI-CNT
                       PERFORM 10012-RSI-MCNT-SEC
                       MOVE    WRK-RSI-CNT         TO  WRK-SANTEI-CNTW
                   END-IF
                   ADD     WRK-SANTEI-CNTW         TO  WRK-SANTEI-CNT
      *
      *            訂正日以降の回数を求める
                   IF     (LNK-SC91-SYORIFLG   =   1    )  AND
                          (SPA-SRYYMD(1:6)     =   SANTEI-SRYYM)
                       MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                       ADD     1                   TO  IDX-Y2
                       PERFORM VARYING     IDY     FROM    IDX-Y2  BY  1
                               UNTIL       IDY     >   31
                           ADD     SANTEI-DAY (IDY)    TO  WRK-ATO-CNT
                       END-PERFORM
                   END-IF
               END-IF
      *!!!
           END-IF
      *!!!
      *    上限他月数が９９の時は、履歴があれば終了
               IF      TNS-JGNCNTETCM      =   99
                   IF      WRK-SANTEI-CNT      >=  TNS-JGNCNTETC
                       MOVE    1                   TO  FLG-SANTEI
                   END-IF
               END-IF
      *
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
      *    算定歴の回数をチェックする
           MOVE    TNS-JGNCNTETC       TO  LNK-SC91-MAXCNT
      *    訂正の時、訂正分はカウントしない
           IF      LNK-SC91-SYORIFLG   =   1
               IF      WRK-SANTEI-CNT      >=   WRK-TEI-CNT
                   COMPUTE WRK-SANTEI-CNT      =   WRK-SANTEI-CNT
                                               -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO                TO  WRK-SANTEI-CNT
               END-IF
           END-IF
           MOVE    WRK-SANTEI-CNT      TO  LNK-SC91-CNT
           MOVE    TNS-JGNCNTETCM      TO  WRK-JGNCNTETCM
      *
           .
       1002-SANTEI-ETC-EXT.
           EXIT.
      *****************************************************************
      *    外来まとめ月数処理
      *****************************************************************
       10029-KT01SANTEI-ETC-SEC         SECTION.
      *
      *    外来まとめ算定履歴編集処理
           PERFORM 10091-KT01SANTEI-HEN-SEC
      *
           ADD     SANTEI-MSANTEICNT   TO  LNK-SC91-CNT
      *
           IF      SANTEI-MSANTEICNT   >   ZERO
               MOVE    SPA-SRYYMD(1:6)     TO  WRK-CHK-SRYYM
               MOVE    SANTEI-MSANTEID     TO  WRK-CHK-SRYDD
           END-IF
      *
           .
       10029-KT01SANTEI-ETC-EXT.
           EXIT.
      *****************************************************************
      *    今回診療行為チェック処理
      *****************************************************************
       1003-INPUT-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-DOCNT
           MOVE    ZERO                TO  WRK-DODAYCNT
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
           MOVE    LNK-SC91-SRYCD      TO  WRK-SRYCD
           MOVE    1                   TO  IDX-ST
           MOVE    IDX-Y2              TO  IDX-ED
      *    画面内算定集計
           PERFORM 10031-INPUT-CNT-SEC
           .
       1003-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    今回診療行為チェック処理
      *****************************************************************
       10031-INPUT-CNT-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-CHK-DD
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
      *
               MOVE    ZERO                TO  FLG-TBL-CHK
               IF     (SPA-COM-HKNCOMBI (IDX)  =   "9999")
                   MOVE    1                   TO  FLG-TBL-CHK
               END-IF
               IF      (SPA-COM-INPUTCD(IDX)
                                           =   WRK-SRYCD  )  OR
                       (SPA-COM-CHK-SRYCD (IDX)
                                           =   WRK-SRYCD  )
                   CONTINUE
               ELSE
                   MOVE    1                   TO  FLG-TBL-CHK
               END-IF
      *
      *        入院で１日上限の時、入力日のみ
      *        IF     (SPA-NYUGAIKBN       =   "1"   )  AND
      *               (TNS-JGNCNT1D        >   ZERO  )  AND
      *               (SPA-COM-NYU-KAI(IDX)    >   ZERO )
      *         AND   (FLG-TBL-CHK         =   ZERO  )
      *            IF      SPA-COM-NYUINDAY(IDX IDX-Y2)    =   ZERO
      *                MOVE    1               TO  FLG-TBL-CHK
      *            END-IF
      *        END-IF
      *
               IF      FLG-TBL-CHK         =   ZERO
                   IF    ( LNK-SC91-KBN        =   "1"  OR  "3"     )
                   AND   ( IDX                 =   LNK-SC91-GMN-IDX )
      *                チェック行、外来は対象外
                       IF     (SPA-NYUGAIKBN       =   "2"   )
                           MOVE    1               TO  FLG-TBL-CHK
                       END-IF
                   END-IF
               END-IF
      *
               IF      FLG-TBL-CHK         =   ZERO
                   IF      (TNS-JGNCNT1D       >   ZERO ) AND
                           (TNS-TANICD     NOT =   "001")
      *                日上限の数量を回数とする
                       COMPUTE WRK-DCNT        =  (SPA-COM-KAISU(IDX) 
                                               *   SPA-COM-SURYO(IDX))
      *H24.8
                       IF     (SPA-COM-KAISU(IDX)
                                               *   SPA-COM-SURYO(IDX))
                                               >   999
                           MOVE    999             TO  WRK-DCNT
                       END-IF
      *H24.8
                   ELSE
                       MOVE    SPA-COM-KAISU(IDX)  TO  WRK-DCNT
                   END-IF
                   IF     (SPA-NYUGAIKBN       =   "2"   )
      *            外来
                       ADD     WRK-DCNT            TO  WRK-DOCNT
                       MOVE    SPA-COM-CHK-SRYCD (IDX)
                                                   TO  WRK-CHK-SRYCD
                       ADD     WRK-DCNT            TO  WRK-DODAYCNT
                   ELSE
      *            入院
                       IF     (LNK-SC91-KBN        =   "1"  OR  "3"  )
                         AND  (IDX                 =   LNK-SC91-GMN-IDX)
      *                    対象行は前日までの集計
                           COMPUTE IDX-ED2             =   IDX-ED
                                                       -   1
                       ELSE
                           MOVE    IDX-ED              TO  IDX-ED2
                       END-IF
                       PERFORM VARYING IDD     FROM    IDX-ST   BY  1
                               UNTIL  (IDD             >  IDX-ED2   )
                           IF      SPA-COM-NYUINDAY(IDX IDD) >   ZERO
                               ADD     WRK-DCNT        TO  WRK-DOCNT
                               MOVE    SPA-COM-CHK-SRYCD (IDX)
                                                       TO  WRK-CHK-SRYCD
                               MOVE    IDD             TO  WRK-CHK-DD
                               IF      IDX-Y2          =   IDD
                                   ADD     WRK-DCNT    TO  WRK-DODAYCNT
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-PERFORM
           .
       10031-INPUT-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    同時算定コード存在のチェック処理
      *****************************************************************
       1001-CODECHK-SEC      SECTION.
      *
           SET     TBL-IDX            TO  1
           SEARCH      TBL-DOUJI-OCC    VARYING   TBL-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-CODE
                       MOVE    SPACE           TO  WRK-CHK
                   WHEN    LNK-SC91-SRYCD      =   TBL-SRYCD (TBL-IDX)
                       MOVE    TBL-CODE   (TBL-IDX)    TO  WRK-CODE
                       MOVE    TBL-CHK    (TBL-IDX)    TO  WRK-CHK
           END-SEARCH
      *
      *    Ｈ１８.１０から消炎鎮痛等処置と同時チェック
           IF      WRK-CHK             =   "H"
               IF      SPA-SRYYMD      >=  "20061001"
                   MOVE    SPACE           TO  WRK-CHK
               ELSE
                   MOVE    SPACE           TO  WRK-CODE
                   MOVE    SPACE           TO  WRK-CHK
               END-IF
           END-IF
      *
           IF      SPA-SRYYMD          >=  "20100401"
               IF     (WRK-CODE            =   "002" )  AND
                      (WRK-CHK             =   "1"   )
                   MOVE    SPACE           TO  WRK-CODE
                   MOVE    SPACE           TO  WRK-CHK
               END-IF
           END-IF
      *
           .
       1001-CODECHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同時算定歴チェック処理
      *****************************************************************
       200-DOUJI-CHK-SEC      SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  TNSWK-TENSU-REC
      *
           PERFORM 2000-SANTEI-HZN-SEC
      *H25.1
           MOVE    WRK-CHK-SRYCD       TO  WRK-CHK2-SRYCD
           MOVE    WRK-CHK-SRYYMD      TO  WRK-CHK2-SRYYMD
           MOVE    WRK-CHK-NAME        TO  WRK-CHK2-NAME
      *
           MOVE    SPACE               TO  WRK-CHK-SRYCD
           MOVE    SPACE               TO  WRK-CHK-NAME
           INITIALIZE                      WRK-CHK-SRYYMD
           MOVE    ZERO                TO  WRK-CNT
           MOVE    ZERO                TO  WRK-DOCNT-SUM
           MOVE    ZERO                TO  WRK-DAYCNT-SUM
           MOVE    ZERO                TO  WRK-DOCNT-P
           MOVE    ZERO                TO  WRK-DAYCNT-P
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   TBL-MAX
               MOVE    ZERO                TO  WRK-DOCNT
               MOVE    ZERO                TO  WRK-DAYCNT
               SET     TBL-IDX         TO  IDX2
      *        労災で、消炎鎮痛の時、湿布対象外
               IF     ((SPA-HKNKBN         =   1    )  AND
                       (SPA-RSI-JUNKYO  NOT =   "1" )) AND
                       (WRK-CHK        NOT =   SPACE   )  AND
                       (WRK-CHK            =   TBL-CHK (TBL-IDX))
                   MOVE    1                   TO  FLG-ERR
               ELSE
                   MOVE    ZERO                TO  FLG-ERR
               END-IF
      *        H22.4 から手帳加算は対象外
               IF      SPA-SRYYMD          >=  "20100401"
                   IF     (WRK-CODE            =   "002" )
                     AND  (WRK-CODE            =   TBL-CODE (TBL-IDX))
                     AND  (TBL-CHK (TBL-IDX)   =   "1"   )
                       MOVE    1                   TO  FLG-ERR
                   END-IF
               END-IF
      *
               IF      (WRK-CODE           =   TBL-CODE  (TBL-IDX))
                   AND (LNK-SC91-SRYCD NOT =   TBL-SRYCD(TBL-IDX) )
                   AND (FLG-ERR            =   ZERO    )
                   MOVE    TBL-SRYCD (TBL-IDX)     TO  WRK-SRYCD
                   MOVE    WRK-SRYCD               TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF     (FLG-TENSU           =   1    )
      *DD            OR   (TNS-SANTEIRRKKBN    =   ZERO )
                       CONTINUE
                   ELSE
                       IF      LNK-SC91-SYORIFLG   =   1
                           PERFORM 1001-TEISEI-CNT-SEC
                       ELSE
                           MOVE    ZERO            TO  WRK-TEI-CNT
                      END-IF
      *
                      IF      LNK-SC91-KBN        =   "4"
                           CONTINUE
                      ELSE
      *                    今回入力分カウント
                           PERFORM 2003-DOJINPUT-CHK-SEC
                      END-IF
                      IF     ((SPA-HKNKBN          =   1 ) AND
                              (SPA-RSI-JUNKYO  NOT =   "1" )) AND
                              (TBL-CHK (TBL-IDX)   =   "P"  )
      *                    労災で湿布の時
                           IF      WRK-DOCNT       >   ZERO
                               MOVE    1           TO  WRK-DOCNT-P
                           END-IF
                      ELSE
                           ADD     WRK-DOCNT       TO  WRK-DOCNT-SUM
                           ADD     WRK-DODAYCNT    TO  WRK-DAYCNT-SUM
                      END-IF
      *
      *               上限他月数のみ設定
                      IF     (TNS-JGNCNTETCM     >   ZERO )
                        AND  (TNS-JGNCNT         =   ZERO )
                        AND  (TNS-JGNCNT1D       =   ZERO )
                          PERFORM 2002-DOJSANTEI-ETC-SEC
                      ELSE
                          PERFORM 2001-DOJSANTEI-YMD-SEC
                      END-IF
                      IF     ((SPA-HKNKBN          =   1 ) AND
      ***                     (SPA-RSI-HKNKBN  NOT =   "5" )) AND
                              (SPA-RSI-JUNKYO  NOT =   "1" )) AND
                              (TBL-CHK (TBL-IDX)   =   "P"     )
      *                    労災で湿布の時
                           IF      WRK-DAYCNT      >   ZERO
                               MOVE    1           TO  WRK-DOCNT-P
                           END-IF
                      ELSE
                           ADD     WRK-DAYCNT      TO  WRK-DAYCNT-SUM
                      END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *H25.1
      *    最後に算定した日を表示対象とする
           IF      WRK-CHK2-SRYYMD     >   WRK-CHK-SRYYMD
               MOVE    WRK-CHK2-SRYYMD     TO  WRK-CHK-SRYYMD
           END-IF
      *
           COMPUTE WRK-DOCNT-SUM       =   WRK-DOCNT-SUM
                                       +   WRK-DOCNT-P
      *
           COMPUTE WRK-SC91-CHKCNT     =   WRK-SC91-CNT
                                       +   WRK-DOCNT-SUM
                                       +   WRK-CNT
           COMPUTE WRK-SC91-DAYCHKCNT  =   WRK-SC91-DAYCNT
                                       +   WRK-DAYCNT-SUM
      *                                +   WRK-DOCNT-SUM
      *    週上限なし
           MOVE    ZERO                TO  WRK-SC91-WEEKCHKCNT
      *
      *    保険毎のコードの判定
           MOVE    LNK-SC91-SRYCD      TO  WRK-SRYCD
           PERFORM 1000-SANTEI-CHK-SEC
      *
           IF      WRK-CHK-NAME        =   SPACE
               MOVE    WRK-CHK-SRYCD       TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               MOVE    TNS-NAME            TO  WRK-CHK-NAME
           END-IF
      *H25.1
      *    人工腎臓は名称編集なしとする
           IF      WRK-CODE            =   "008"
               MOVE    SPACE               TO  WRK-CHK-NAME
           END-IF
      *
      *    上限回数チェック・エラー編集
           MOVE    1                   TO  FLG-DOUCHK
           PERFORM 1009-CNTCHK-SYORI-SEC
      *
      *    点数マスタの内容を入力分へ戻す
           MOVE    TNSWK-TENSU-REC     TO  TNS-TENSU-REC
      *
           .
       200-DOUJI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数チェック処理（診療年月）
      *****************************************************************
       2001-DOJSANTEI-YMD-SEC      SECTION.
      *
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *    算定履歴テーブル検索
           MOVE    ZERO                TO  FLG-SANTEI-ARI
           INITIALIZE                  SANTEI-REC
           PERFORM VARYING     IDX-SAN2    FROM    1   BY  1
                   UNTIL      (IDX-SAN2    >   IDX-SAN )
                           OR (FLG-SANTEI-ARI  =   1   )
               IF      HZN-SANTEI-SRYCD (IDX-SAN2)
                                           =  WRK-SRYCD
                   MOVE    HZN-SANTEI-REC (IDX-SAN2)
                                           TO  SANTEI-REC
                   MOVE    1               TO  FLG-SANTEI-ARI
               END-IF
           END-PERFORM
      *
      *    算定歴の回数をチェックする
           IF      TNS-JGNCNT          >   ZERO
               ADD     SANTEI-MSANTEICNT   TO  WRK-CNT
           END-IF
      *    労災でリハビリの時３月以内の件数はカウントしない
           IF     ((SPA-HKNKBN          =   1 ) AND
                   (SPA-RSI-JUNKYO  NOT =   "1" )) AND
                  (FLG-HKN-SANTEI      =   1     )
               MOVE    WRK-CNT             TO  WRK-RSI-CNT
               PERFORM 10012-RSI-MCNT-SEC
               MOVE    WRK-RSI-CNT         TO  WRK-CNT
           END-IF
      *    上限１日
           IF     (TNS-JGNCNT1D        >   ZERO  )  AND
                  (SPA-SRYYMD(7:2)     NUMERIC   )  AND
                  (SPA-SRYYMD(7:2)     >   ZERO  )
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               ADD     SANTEI-DAY (IDX-Y2) TO  WRK-DAYCNT
           END-IF
      *
      *    訂正の時、訂正分はカウントしない
           IF      LNK-SC91-SYORIFLG   =   1
               IF      WRK-DAYCNT          >=  WRK-TEI-CNT
                   COMPUTE WRK-DAYCNT          =   WRK-DAYCNT
                                               -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO            TO  WRK-DAYCNT
               END-IF
               IF      WRK-CNT              >=  WRK-TEI-CNT
                   COMPUTE WRK-CNT          =   WRK-CNT
                                            -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO            TO  WRK-CNT
               END-IF
           END-IF
      *
           IF     (WRK-CNT             >   ZERO  )  OR
                  (WRK-DAYCNT          >   ZERO  )
               IF     (SANTEI-MSANTEID     >   ZERO )  AND
                      (SANTEI-SRYYM        >   ZERO )
                   MOVE    WRK-SRYCD           TO  WRK-CHK-SRYCD
                   MOVE    SANTEI-SRYYM        TO  WRK-CHK-SRYYM
      *??          MOVE    SANTEI-MSANTEID     TO  WRK-CHK-SRYDD
      *??          MOVE    TNS-NAME            TO  WRK-CHK-NAME
                   IF      TNS-JGNCNT          >   ZERO
                       PERFORM VARYING     IDX-Y2  FROM    31  BY  -1
                               UNTIL       IDX-Y2  <   1
                           IF     (SANTEI-DAY (IDX-Y2)    >   ZERO )
      *H25.1                   最後の算定日を対象
                               IF    (IDX-Y2       >   WRK-CHK-SRYDD)
                                   MOVE    TNS-NAME    TO  WRK-CHK-NAME
                                   MOVE    IDX-Y2      TO  WRK-CHK-SRYDD
                               END-IF
                               MOVE    1           TO  IDX-Y2
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-IF
           .
       2001-DOJSANTEI-YMD-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定回数チェック処理（月数回数）
      *****************************************************************
       2002-DOJSANTEI-ETC-SEC      SECTION.
      *
      *    算定開始年月
           MOVE    SPA-SRYYMD(1:6)     TO  WRK-STRYM
      *
           MOVE    TNS-JGNCNTETCM      TO  WRK-CHK-JGNCNTETCM
      *
           IF      TNS-JGNCNTETCM      =   99
      *        上限他月数が９９の時は、全履歴１回とする
               MOVE    ZERO                TO  WRK-STRYM
           ELSE
      *
      *        最長５年とする
               IF      WRK-CHK-JGNCNTETCM  >   60
                   MOVE    60                  TO  WRK-CHK-JGNCNTETCM
               END-IF
      *
               PERFORM UNTIL   WRK-CHK-JGNCNTETCM  <=  12
                   COMPUTE WRK-STRYY       =   WRK-STRYY    -   1
                   COMPUTE WRK-CHK-JGNCNTETCM  =   WRK-CHK-JGNCNTETCM
                                       -   12
               END-PERFORM
      *
               IF      WRK-STRMM           <   WRK-CHK-JGNCNTETCM
                   COMPUTE WRK-STRYY       =   WRK-STRYY    -   1
                   COMPUTE WRK-STRMM       =   WRK-STRMM
                                           +   12
                                       -  (WRK-CHK-JGNCNTETCM   -  1 )
               ELSE
                   COMPUTE WRK-STRMM       =   WRK-STRMM
                                       -  (WRK-CHK-JGNCNTETCM   -  1 )
               END-IF
           END-IF
      *
      *    保険毎のコードの判定
           PERFORM 1000-SANTEI-CHK-SEC
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           IF     (FLG-HKNSAN          =   1   )  AND
                  (SPA-HKNKBN      NOT =   ZERO)
               MOVE    SPA-HKNCOMBINUM     TO  SANTEI-HKNCOMBINUM
           ELSE
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           END-IF
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
               INITIALIZE                  SANTEI-REC
           END-IF
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
           PERFORM UNTIL   FLG-SANTEI      =   1
               IF      SANTEI-SRYYM    <   WRK-STRYM
                   MOVE    1               TO  FLG-SANTEI
               ELSE
                   IF      WRK-CHK-SRYYM   =   SPACE
                       MOVE    SANTEI-SRYYM        TO  WRK-CHK-SRYYM
                       MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                       ADD     1                   TO  IDX-Y2
                       PERFORM VARYING     IDX-Y2  FROM    31  BY  -1
                               UNTIL       IDX-Y2  <   1
                           IF      SANTEI-DAY (IDX-Y2)    >   ZERO
                               MOVE    IDX-Y2      TO  WRK-CHK-SRYDD
                               MOVE    1           TO  IDX-Y2
                           END-IF
                       END-PERFORM
                   END-IF
                   MOVE    SANTEI-MSANTEICNT   TO  WRK-SANTEI-CNTW
      *            労災でリハビリの時３月以内の件数はカウントしない
      *********    IF     (SPA-HKNKBN      NOT =   ZERO  )  AND
                   IF     (SPA-HKNKBN          =   1     )  AND
                          (FLG-HKN-SANTEI      =   1     )
                       MOVE    WRK-SANTEI-CNTW     TO  WRK-RSI-CNT
                       PERFORM 10012-RSI-MCNT-SEC
                       MOVE    WRK-RSI-CNT         TO  WRK-SANTEI-CNTW
                   END-IF
                   ADD     WRK-SANTEI-CNTW         TO  WRK-SANTEI-CNT
      *
                   IF      WRK-SANTEI-CNTW     >   ZERO
                       MOVE    SANTEI-SRYYM        TO  WRK-CHK-SRYYM
                       MOVE    SANTEI-MSANTEID     TO  WRK-CHK-SRYDD
                       MOVE    WRK-SRYCD           TO  WRK-CHK-SRYCD
                       MOVE    TNS-NAME            TO  WRK-CHK-NAME
                   END-IF
      *
      *            訂正日以降の回数を求める
                   IF     (LNK-SC91-SYORIFLG   =   1    )  AND
                          (SPA-SRYYMD(1:6)     =   SANTEI-SRYYM)
                       MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                       ADD     1                   TO  IDX-Y2
                       PERFORM VARYING     IDY     FROM    IDX-Y2  BY  1
                               UNTIL       IDY     >   31
                           ADD     SANTEI-DAY (IDY)    TO  WRK-ATO-CNT
                       END-PERFORM
                   END-IF
               END-IF
      *    上限他月数が９９の時は、履歴があれば終了
               IF      TNS-JGNCNTETCM      =   99
                   IF      WRK-SANTEI-CNT      >=  TNS-JGNCNTETC
                       MOVE    1                   TO  FLG-SANTEI
                   END-IF
               END-IF
      *
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
      *    訂正の時、訂正分はカウントしない
           IF      LNK-SC91-SYORIFLG   =   1
               IF      WRK-SANTEI-CNT      >=  WRK-TEI-CNT
                   COMPUTE WRK-SANTEI-CNT      =   WRK-SANTEI-CNT
                                               -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO            TO  WRK-SANTEI-CNT
               END-IF
           END-IF
      *
      *    算定歴の回数をチェックする
           ADD     WRK-SANTEI-CNT      TO  WRK-CNT
      *
           .
       2002-DOJSANTEI-ETC-EXT.
           EXIT.
      *****************************************************************
      *    今回診療行為チェック処理
      *****************************************************************
       2003-DOJINPUT-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-DOCNT
           MOVE    ZERO                TO  WRK-DODAYCNT
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
           MOVE    1                   TO  IDX-ST
           MOVE    IDX-Y2              TO  IDX-ED
      *    画面内算定集計
           PERFORM 10031-INPUT-CNT-SEC
      *
           .
       2003-DOJINPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴テーブル編集処理
      *****************************************************************
       2000-SANTEI-HZN-SEC         SECTION.
      *
           INITIALIZE                  HZN-SANTEI-AREA
           MOVE    ZERO                TO  IDX-SAN
      *
           MOVE    SPACE               TO  SANTEI-REC
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM
               UNTIL      (FLG-SANTEI      =   1  )
               SET     TBL-IDX              TO  1
               SEARCH  TBL-DOUJI-OCC        VARYING   TBL-IDX
                   AT  END
                       MOVE    ZERO            TO  FLG-TAISYO
                   WHEN    SANTEI-SRYCD        =   TBL-SRYCD (TBL-IDX)
                       MOVE    1               TO  FLG-TAISYO
               END-SEARCH
               IF      FLG-TAISYO          =   1
      *            保険毎のコードの判定
                   MOVE    SANTEI-SRYCD            TO  WRK-SRYCD
                   PERFORM 1000-SANTEI-CHK-SEC
                   IF     (FLG-HKNSAN          =   1   )  AND
                          (SPA-HKNKBN      NOT =   ZERO)
                       IF      SPA-HKNCOMBINUM =   SANTEI-HKNCOMBINUM
                           MOVE    1               TO  FLG-TAISYO
                       ELSE
                           MOVE    ZERO            TO  FLG-TAISYO
                       END-IF
                   ELSE
                       IF      SANTEI-HKNCOMBINUM  NOT =   ZERO
                           MOVE    ZERO            TO  FLG-TAISYO
                       END-IF
                   END-IF
               END-IF
               IF      FLG-TAISYO          =   1
                   ADD     1                   TO  IDX-SAN
                   MOVE    SANTEI-REC          TO  HZN-SANTEI-REC
                                                            (IDX-SAN)
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2000-SANTEI-HZN-EXT.
           EXIT.
      *****************************************************************
      *    入院期間の算定回数処理
      *****************************************************************
       2001-NYUIN-CHEK-SEC      SECTION.
      *
           IF      LNK-SC91-KBN        =   "8"
      *        継続初回入院日から
               IF      SPA-STR-NYUINYMD    =   SPACE
                   MOVE    SPA-K02N-NYUINYMD   TO  WRK-NYUINYMD
               ELSE
                   MOVE    SPA-STR-NYUINYMD    TO  WRK-NYUINYMD
               END-IF
           ELSE
      *        入院日
               MOVE    SPA-K02N-NYUINYMD   TO  WRK-NYUINYMD
           END-IF
      *    入院期間上限回数
           IF      LNK-SC91-NYUCNT     =   ZERO
               MOVE    1                   TO  CNT-NYU
           ELSE
               MOVE    LNK-SC91-NYUCNT     TO  CNT-NYU
           END-IF
      *
           MOVE    LNK-SC91-SRYCD      TO  WRK-SRYCD
      *    訂正の時、前回算定時の算定回数
           IF      LNK-SC91-SYORIFLG   =   1
               MOVE    LNK-SC91-SRYCD      TO  WRK-SRYCD
               PERFORM 1001-TEISEI-CNT-SEC
           ELSE
               MOVE    ZERO            TO  WRK-TEI-CNT
           END-IF
      *
      *    算定履歴検索（算定履歴区分は１とする）
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    ZERO                TO  FLG-SYOKAI-OK
           MOVE    ZERO                TO  WRK-NYU
           MOVE    ZERO                TO  WRK-NYU2
           INITIALIZE                      WRK-CHK-SRYYMD
           PERFORM UNTIL   (FLG-SANTEI     =   1   )  OR
                           (FLG-SYOKAI-OK  =   1   )
               MOVE    SANTEI-SRYYM        TO  WRK-YM
               MOVE    ZERO                TO  WRK-DD
               PERFORM VARYING    ID2      FROM    1   BY  1
                       UNTIL     (ID2      >   31  )  OR
                                 (FLG-SYOKAI-OK    =   1   )
                   IF      SANTEI-DAY (ID2)    NOT =   ZERO
                       MOVE    ID2             TO  WRK-DD
      *ver.4.7
                       IF    ((WRK-NYUINYMD        =  WRK-YMD)  AND
                              (SPA-K02N-DOUDAY NOT =  ZERO   ))
                       OR    ((SPA-K02N-TAIINYMD   =  WRK-YMD)  AND
                              (SPA-K02N-DOUTAIKBN  =  "1"    ))
      *                    同日再入院日の算定
                           PERFORM 20011-DOUNYUIN-SEC
                           IF      SANTEI-DAY (ID2)    >=  WRK-ACT-DAY
                               COMPUTE SANTEI-DAY (ID2)
                                               =   SANTEI-DAY (ID2)
                                               -   WRK-ACT-DAY
                           ELSE
                               MOVE    ZERO    TO  SANTEI-DAY (ID2)
                           END-IF
                       END-IF
      *ver.4.7
      *                訂正の時当日分は算定なしとする
                       IF     (LNK-SC91-SYORIFLG   =   1  )  AND
                              (WRK-YMD             =   SPA-SRYYMD) AND
                              (WRK-TEI-CNT         >   ZERO )
                           IF      SANTEI-DAY (ID2)    >=  WRK-TEI-CNT
                               COMPUTE SANTEI-DAY (ID2)
                                               =   SANTEI-DAY (ID2)
                                               -   WRK-TEI-CNT
                           ELSE
                               MOVE    ZERO    TO  SANTEI-DAY (ID2)
                           END-IF
                       END-IF
                       IF     (WRK-NYUINYMD
                                               <=  WRK-YMD )  AND
                              (SPA-K02N-TAIINYMD
                                               >=  WRK-YMD )
                           ADD     SANTEI-DAY (ID2)    TO  WRK-NYU
                           IF     (WRK-YMD     <   SPA-K02N-NYUINYMD)
      *                    入院日以前の算定
                               ADD     SANTEI-DAY (ID2)    TO  WRK-NYU2
                           END-IF
                           MOVE    WRK-YMD         TO  WRK-CHK-SRYYMD
                       END-IF
                   END-IF
               END-PERFORM
               IF      SANTEI-SRYYM        <   WRK-NYUINYMD (1:6)
                   MOVE    1                   TO  FLG-SANTEI
               ELSE
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
           MOVE    WRK-NYU             TO  LNK-SC91-CNT
           MOVE    WRK-NYU2            TO  LNK-SC91-NYUZCNT
           IF     (WRK-NYU             >=  CNT-NYU   )
               MOVE    "9031"              TO  LNK-SC91-ERRCD
      *        入院日以前は警告
               IF     (WRK-NYU2            >   ZERO )
      *            入院日以前は警告
                   MOVE    "KXXX"              TO  LNK-SC91-ERRCD
               END-IF
           END-IF
           .
       2001-NYUIN-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院日の算定チェック処理
      *****************************************************************
       20011-DOUNYUIN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-ACT-DAY
      *
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  SRY-SRYYM
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(1)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(2)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(3)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(4)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(5)
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL     (FLG-SRYACT   =   1 )
      *        診療会計マスタ読み込み
               MOVE    SPACE               TO  SRYACCT-REC
               INITIALIZE                      SRYACCT-REC
               MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    SRY-PTID            TO  ACCT-PTID
               MOVE    SRY-SRYKA           TO  ACCT-SRYKA
               MOVE    SRY-SRYYM           TO  ACCT-SRYYM
               MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
               MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 940-SRYACCT-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
                   INITIALIZE                      SRYACCT-REC
               END-IF
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *        退院日算定分
               IF      WRK-NYUINYMD        =  WRK-YMD
                   IF      ACCT-DAY (2 ID2)    >   ZERO
                       ADD     ACCT-DAY (2 ID2)    TO  WRK-ACT-DAY
                   END-IF
               END-IF
               IF      SPA-K02N-TAIINYMD   =  WRK-YMD
                   IF      ACCT-DAY (3 ID2)    >   ZERO
                       ADD     ACCT-DAY (3 ID2)    TO  WRK-ACT-DAY
                   END-IF
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20011-DOUNYUIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量全角変換処理
      *****************************************************************
       800-SURYO-HEN-SEC      SECTION.
      *
           MOVE    WRK-HEN-ZZ          TO  WRK-HEN-CNT
           MOVE    SPACE               TO  WRK-MOJI
           PERFORM VARYING     IDXM    FROM    1   BY  1
                   UNTIL       IDXM    >   3
               IF      WRK-HEN-CNT (IDXM:1) NOT =   SPACE
                   MOVE    WRK-HEN-CNT(IDXM:)   TO  WRK-MOJI
                   MOVE    3                    TO  IDXM
               END-IF
           END-PERFORM
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MOJI            TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO )  AND
                  (KANACHK-MAX         >   ZERO )
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-HEN-JCNT
           ELSE
               MOVE    WRK-MOJI            TO  WRK-HEN-JCNT
           END-IF
           .
       800-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
      *    算定履歴に関する項目のみ
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key22"             TO  MCP-PATHNAME
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
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    附加情報の上限回数設定
           IF      FLG-TENSU           =   ZERO
               PERFORM 9101-TENSUPLUS-SYORI-SEC
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       9101-TENSUPLUS-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   MOVE    1                   TO  FLG-TENSUPLUS
                   INITIALIZE                      TENSUPLUS-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    上限回数
           IF      TNSP-SANTEIRRKKBN   =   1
               MOVE    TNSP-SANTEIRRKKBN   TO  TNS-SANTEIRRKKBN
               MOVE    TNSP-JGNCNT         TO  TNS-JGNCNT
               MOVE    TNSP-JGNCNT1D       TO  TNS-JGNCNT1D
               MOVE    TNSP-JGNCNT1W       TO  TNS-JGNCNT1W
               MOVE    TNSP-JGNCNTERR      TO  TNS-JGNCNTERR
      *
               MOVE    TNSP-JGNCNTETCM     TO  TNS-JGNCNTETCM
               MOVE    TNSP-JGNCNTETC      TO  TNS-JGNCNTETC
           END-IF
      *
           .
       9101-TENSUPLUS-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
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
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-READ-SEC         SECTION.
      *
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
       930-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ読み込み
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
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
       940-SRYACCT-READ-EXT.
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
