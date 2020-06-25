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
       PROGRAM-ID.                     ORCGW63.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : データチェック機能情報３
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/04/01    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
       01  SPA-W63.
           03  SPA-GMN-AREA.
               05  SPA-GMN-CUR         PIC 9(03).
               05  SPA-GMN-GMN-AREA.
                   07  SPA-GMN-CLIST-AREA.
                       09  SPA-GMN-CLIST-MAX       PIC 9(03).
                       09  SPA-GMN-CHECKLIST       OCCURS  200.
                           11  SPA-GMN-TNUM        PIC 9(3).
                           11  SPA-GMN-THKNJANUM   PIC X(08).
                           11  SPA-GMN-TJYKNKBN    PIC X(04).
                           11  SPA-GMN-THKNJANAME  PIC X(60).
                           11  SPA-GMN-THKNNUM     PIC X(20).
                           11  SPA-GMN-TKIGOCHK    PIC X(10).
                           11  SPA-GMN-TSTYUKYMD   PIC X(09).
                           11  SPA-GMN-TEDYUKYMD   PIC X(09).
                   07  SPA-GMN-SELNUM              PIC 9(03).
                   07  SPA-GMN-INPUT-AREA.
                       09  SPA-GMN-HKNJANUM        PIC X(10).
                       09  SPA-GMN-HKNJANAME       PIC X(100).
                       09  SPA-GMN-JYKNKBN-G.
                           11  SPA-GMN-JYKNKBN     PIC X(01).
                           11  SPA-GMN-FL01        PIC X(01).
                           11  SPA-GMN-JYKNKBNMEI  PIC X(14).
                       09  SPA-GMN-HKNNUM-G.
                           11  SPA-GMN-HKNNUM      PIC X(03).
                           11  SPA-GMN-FL02        PIC X(01).
                           11  SPA-GMN-HKNNUMMEI   PIC X(16).
                       09  SPA-GMN-KGCHKPTN-G.
                           11  SPA-GMN-KGCHKPTN    PIC X(01).
                           11  SPA-GMN-FL03        PIC X(01).
                           11  SPA-GMN-KGCHKPTNMEI PIC X(38).
                       09  SPA-GMN-KGSUJI-G.
                           11  SPA-GMN-KGSUJI      PIC X(01).
                           11  SPA-GMN-FL04        PIC X(01).
                           11  SPA-GMN-KGSUJIMEI   PIC X(18).
                       09  SPA-GMN-KGKANJI-G.
                           11  SPA-GMN-KGKANJI     PIC X(01).
                           11  SPA-GMN-FL05        PIC X(01).
                           11  SPA-GMN-KGKANJIMEI  PIC X(18).
                       09  SPA-GMN-KGABT-G.
                           11  SPA-GMN-KGABT       PIC X(01).
                           11  SPA-GMN-FL06        PIC X(01).
                           11  SPA-GMN-KGABTMEI    PIC X(18).
                       09  SPA-GMN-KGHYPN-G.
                           11  SPA-GMN-KGHYPN      PIC X(01).
                           11  SPA-GMN-FL07        PIC X(01).
                           11  SPA-GMN-KGHYPNMEI   PIC X(18).
                       09  SPA-GMN-KGKAKKO-G.
                           11  SPA-GMN-KGKAKKO     PIC X(01).
                           11  SPA-GMN-FL08        PIC X(01).
                           11  SPA-GMN-KGKAKKOMEI  PIC X(18).
                       09  SPA-GMN-KGETC-G.
                           11  SPA-GMN-KGETC       PIC X(01).
                           11  SPA-GMN-FL09        PIC X(01).
                           11  SPA-GMN-KGETCMEI    PIC X(18).
                       09  SPA-GMN-KGKANJINASHI-G.
                           11  SPA-GMN-KGKANJINASHI PIC X(01).
                           11  SPA-GMN-FL10         PIC X(01).
                           11  SPA-GMN-KGKANJINASHIMEI
                                                   PIC X(18).
                       09  SPA-GMN-KGKETACHK-G.
                           11  SPA-GMN-KGKETACHK    PIC X(01).
                           11  SPA-GMN-FL46         PIC X(01).
                           11  SPA-GMN-KGKETACHKMEI PIC X(18).
                       09  SPA-GMN-KGKETAMIN-G.
                           11  SPA-GMN-KGKETAMIN    PIC X(02).
                           11  SPA-GMN-FL47         PIC X(01).
                           11  SPA-GMN-KGKETAMINMEI PIC X(18).
                       09  SPA-GMN-NM1SUJI-G.
                           11  SPA-GMN-NM1SUJI     PIC X(01).
                           11  SPA-GMN-FL11        PIC X(01).
                           11  SPA-GMN-NM1SUJIMEI  PIC X(18).
                       09  SPA-GMN-NM1KANJI-G.
                           11  SPA-GMN-NM1KANJI    PIC X(01).
                           11  SPA-GMN-FL12        PIC X(01).
                           11  SPA-GMN-NM1KANJIMEI PIC X(18).
                       09  SPA-GMN-NM1ABT-G.
                           11  SPA-GMN-NM1ABT      PIC X(01).
                           11  SPA-GMN-FL13        PIC X(01).
                           11  SPA-GMN-NM1ABTMEI   PIC X(18).
                       09  SPA-GMN-NM1HYPN-G.
                           11  SPA-GMN-NM1HYPN     PIC X(01).
                           11  SPA-GMN-FL14        PIC X(01).
                           11  SPA-GMN-NM1HYPNMEI  PIC X(18).
                       09  SPA-GMN-NM1KAKKO-G.
                           11  SPA-GMN-NM1KAKKO    PIC X(01).
                           11  SPA-GMN-FL15        PIC X(01).
                           11  SPA-GMN-NM1KAKKOMEI PIC X(18).
                       09  SPA-GMN-NM1ETC-G.
                           11  SPA-GMN-NM1ETC      PIC X(01).
                           11  SPA-GMN-FL16        PIC X(01).
                           11  SPA-GMN-NM1ETCMEI   PIC X(18).
                       09  SPA-GMN-NMSUJINASHI-G.
                           11  SPA-GMN-NMSUJINASHI PIC X(01).
                           11  SPA-GMN-FL17        PIC X(01).
                           11  SPA-GMN-NMSUJINASHIMEI PIC X(18).
                       09  SPA-GMN-NMKETACHK-G.
                           11  SPA-GMN-NMKETACHK    PIC X(01).
                           11  SPA-GMN-FL45         PIC X(01).
                           11  SPA-GMN-NMKETACHKMEI PIC X(18).
                       09  SPA-GMN-NMKETAMIN-G.
                           11  SPA-GMN-NMKETAMIN    PIC X(02).
                           11  SPA-GMN-FL18         PIC X(01).
                           11  SPA-GMN-NMKETAMINMEI PIC X(18).
                       09  SPA-GMN-NM2SUJI-G.
                           11  SPA-GMN-NM2SUJI     PIC X(01).
                           11  SPA-GMN-FL20        PIC X(01).
                           11  SPA-GMN-NM2SUJIMEI  PIC X(18).
                       09  SPA-GMN-NM2KANJI-G.
                           11  SPA-GMN-NM2KANJI    PIC X(01).
                           11  SPA-GMN-FL21        PIC X(01).
                           11  SPA-GMN-NM2KANJIMEI PIC X(18).
                       09  SPA-GMN-NM2ABT-G.
                           11  SPA-GMN-NM2ABT      PIC X(01).
                           11  SPA-GMN-FL22        PIC X(01).
                           11  SPA-GMN-NM2ABTMEI   PIC X(18).
                       09  SPA-GMN-NM2HYPN-G.
                           11  SPA-GMN-NM2HYPN     PIC X(01).
                           11  SPA-GMN-FL23        PIC X(01).
                           11  SPA-GMN-NM2HYPNMEI  PIC X(18).
                       09  SPA-GMN-NM2KAKKO-G.
                           11  SPA-GMN-NM2KAKKO    PIC X(01).
                           11  SPA-GMN-FL24        PIC X(01).
                           11  SPA-GMN-NM2KAKKOMEI PIC X(18).
                       09  SPA-GMN-NM2ETC-G.
                           11  SPA-GMN-NM2ETC      PIC X(01).
                           11  SPA-GMN-FL25        PIC X(01).
                           11  SPA-GMN-NM2ETCMEI   PIC X(18).
                       09  SPA-GMN-STYUKYMD        PIC X(09).
                       09  SPA-GMN-EDYUKYMD        PIC X(09).
      *
               05  SPA-GMN-CMB-AREA.
                   07  SPA-GMN-JYKNKBNLST-G.
                       09  SPA-GMN-JYKNKBN-MAX     PIC 9(03).
                       09  SPA-GMN-JYKNKBNLST      OCCURS  5.
                           11  SPA-GMN-JYKNKBNL    PIC X(01).
                           11  SPA-GMN-FL26        PIC X(01).
                           11  SPA-GMN-JYKNKBNMEIL PIC X(14).
                   07  SPA-GMN-HKNNUMLST-G.
                       09  SPA-GMN-HKNNUM-MAX      PIC 9(03).
                       09  SPA-GMN-HKNNUMLST       OCCURS  50.
                           11  SPA-GMN-HKNNUML     PIC X(03).
                           11  SPA-GMN-FL27        PIC X(01).
                           11  SPA-GMN-HKNNUMMEIL  PIC X(16).
                   07  SPA-GMN-KGCHKPTNLST-G.
                       09  SPA-GMN-KGCHKPTN-MAX    PIC 9(03).
                       09  SPA-GMN-KGCHKPTNLST     OCCURS 5.
                           11  SPA-GMN-KGCHKPTNL   PIC X(01).
                           11  SPA-GMN-FL28        PIC X(01).
                           11  SPA-GMN-KGCHKPTNMEIL PIC X(38).
                   07  SPA-GMN-KGSUJILST-G.
                       09  SPA-GMN-KGSUJI-MAX      PIC 9(03).
                       09  SPA-GMN-KGSUJILST       OCCURS 5.
                           11  SPA-GMN-KGSUJIL     PIC X(01).
                           11  SPA-GMN-FL29        PIC X(01).
                           11  SPA-GMN-KGSUJIMEIL  PIC X(18).
                   07  SPA-GMN-KGKANJILST-G.
                       09  SPA-GMN-KGKANJI-MAX     PIC 9(03).
                       09  SPA-GMN-KGKANJILST      OCCURS 5.
                           11  SPA-GMN-KGKANJIL    PIC X(01).
                           11  SPA-GMN-FL30        PIC X(01).
                           11  SPA-GMN-KGKANJIMEIL PIC X(18).
                   07  SPA-GMN-KGABTLST-G.
                       09  SPA-GMN-KGABT-MAX       PIC 9(03).
                       09  SPA-GMN-KGABTLST        OCCURS 5.
                           11  SPA-GMN-KGABTL      PIC X(01).
                           11  SPA-GMN-FL31        PIC X(01).
                           11  SPA-GMN-KGABTMEIL   PIC X(18).
                   07  SPA-GMN-KGHYPNLST-G.
                       09  SPA-GMN-KGHYPN-MAX      PIC 9(03).
                       09  SPA-GMN-KGHYPNLST       OCCURS 5.
                           11  SPA-GMN-KGHYPNL     PIC X(01).
                           11  SPA-GMN-FL32        PIC X(01).
                           11  SPA-GMN-KGHYPNMEIL  PIC X(18).
                   07  SPA-GMN-KGKAKKOLST-G.
                       09  SPA-GMN-KGKAKKO-MAX     PIC 9(03).
                       09  SPA-GMN-KGKAKKOLST      OCCURS 5.
                           11  SPA-GMN-KGKAKKOL    PIC X(01).
                           11  SPA-GMN-FL33        PIC X(01).
                           11  SPA-GMN-KGKAKKOMEIL PIC X(18).
                   07  SPA-GMN-KGETCLST-G.
                       09  SPA-GMN-KGETC-MAX       PIC 9(03).
                       09  SPA-GMN-KGETCLST        OCCURS 5.
                           11  SPA-GMN-KGETCL      PIC X(01).
                           11  SPA-GMN-FL34        PIC X(01).
                           11  SPA-GMN-KGETCMEIL   PIC X(18).
                   07  SPA-GMN-KGKANJINASHILST-G.
                       09  SPA-GMN-KGKANJINASHI-MAX PIC 9(03).
                       09  SPA-GMN-KGKANJINASHILST OCCURS 5.
                           11  SPA-GMN-KGKANJINASHIL PIC X(01).
                           11  SPA-GMN-FL35        PIC X(01).
                           11  SPA-GMN-KGKANJINASHIMEIL
                                                   PIC X(18).
                   07  SPA-GMN-KGKETACHKLST-G.
                       09  SPA-GMN-KGKETACHK-MAX       PIC 9(03).
                       09  SPA-GMN-KGKETACHKLST        OCCURS 5.
                           11  SPA-GMN-KGKETACHKL      PIC X(01).
                           11  SPA-GMN-FL49            PIC X(01).
                           11  SPA-GMN-KGKETACHKMEIL   PIC X(18).
                   07  SPA-GMN-KGKETAMINLST-G.
                       09  SPA-GMN-KGKETAMIN-MAX       PIC 9(03).
                       09  SPA-GMN-KGKETAMINLST        OCCURS 25.
                           11  SPA-GMN-KGKETAMINL      PIC X(02).
                           11  SPA-GMN-FL50            PIC X(01).
                           11  SPA-GMN-KGKETAMINMEIL   PIC X(18).
                   07  SPA-GMN-NMSUJILST-G.
                       09  SPA-GMN-NMSUJI-MAX      PIC 9(03).
                       09  SPA-GMN-NMSUJILST       OCCURS 5.
                           11  SPA-GMN-NMSUJIL     PIC X(01).
                           11  SPA-GMN-FL36        PIC X(01).
                           11  SPA-GMN-NMSUJIMEIL  PIC X(18).
                   07  SPA-GMN-NMKANJILST-G.
                       09  SPA-GMN-NMKANJI-MAX     PIC 9(03).
                       09  SPA-GMN-NMKANJILST      OCCURS 5.
                           11  SPA-GMN-NMKANJIL    PIC X(01).
                           11  SPA-GMN-FL37        PIC X(01).
                           11  SPA-GMN-NMKANJIMEIL PIC X(18).
                   07  SPA-GMN-NMABTLST-G.
                       09  SPA-GMN-NMABT-MAX       PIC 9(03).
                       09  SPA-GMN-NMABTLST        OCCURS 5.
                           11  SPA-GMN-NMABTL      PIC X(01).
                           11  SPA-GMN-FL38        PIC X(01).
                           11  SPA-GMN-NMABTMEIL   PIC X(18).
                   07  SPA-GMN-NMHYPNLST-G.
                       09  SPA-GMN-NMHYPN-MAX      PIC 9(03).
                       09  SPA-GMN-NMHYPNLST       OCCURS 5.
                           11  SPA-GMN-NMHYPNL     PIC X(01).
                           11  SPA-GMN-FL39        PIC X(01).
                           11  SPA-GMN-NMHYPNMEIL  PIC X(18).
                   07  SPA-GMN-NMKAKKOLST-G.
                       09  SPA-GMN-NMKAKKO-MAX     PIC 9(03).
                       09  SPA-GMN-NMKAKKOLST      OCCURS 5.
                           11  SPA-GMN-NMKAKKOL    PIC X(01).
                           11  SPA-GMN-FL40        PIC X(01).
                           11  SPA-GMN-NMKAKKOMEIL PIC X(18).
                   07  SPA-GMN-NMETCLST-G.
                       09  SPA-GMN-NMETC-MAX       PIC 9(03).
                       09  SPA-GMN-NMETCLST        OCCURS 5.
                           11  SPA-GMN-NMETCL      PIC X(01).
                           11  SPA-GMN-FL41        PIC X(01).
                           11  SPA-GMN-NMETCMEIL   PIC X(18).
                   07  SPA-GMN-NMSUJINASHILST-G.
                       09  SPA-GMN-NMSUJINASHI-MAX PIC 9(03).
                       09  SPA-GMN-NMSUJINASHILST  OCCURS 5.
                           11  SPA-GMN-NMSUJINASHIL PIC X(01).
                           11  SPA-GMN-FL42         PIC X(01).
                           11  SPA-GMN-NMSUJINASHIMEIL PIC X(18).
                   07  SPA-GMN-NMKETACHKLST-G.
                       09  SPA-GMN-NMKETACHK-MAX       PIC 9(03).
                       09  SPA-GMN-NMKETACHKLST        OCCURS 5.
                           11  SPA-GMN-NMKETACHKL      PIC X(01).
                           11  SPA-GMN-FL48            PIC X(01).
                           11  SPA-GMN-NMKETACHKMEIL   PIC X(18).
                   07  SPA-GMN-NMKETAMINLST-G.
                       09  SPA-GMN-NMKETAMIN-MAX       PIC 9(03).
                       09  SPA-GMN-NMKETAMINLST        OCCURS 15.
                           11  SPA-GMN-NMKETAMINL      PIC X(02).
                           11  SPA-GMN-FL43            PIC X(01).
                           11  SPA-GMN-NMKETAMINMEIL   PIC X(18).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-CLIST-AREA.
                       09  SPA-NAI-CLIST           OCCURS  200.
                           11  SPA-NAI-TJYKNKBN    PIC X(01).
                           11  SPA-NAI-THKNNUM     PIC X(03).
                           11  SPA-NAI-TSTYUKYMD   PIC X(08).
                           11  SPA-NAI-TEDYUKYMD   PIC X(08).
                           11  SPA-NAI-TKBNCD      PIC X(08).
                   07  SPA-NAI-INPUT-AREA.
                       09  SPA-NAI-STYUKYMD        PIC X(08).
                       09  SPA-NAI-EDYUKYMD        PIC X(08).
                       09  SPA-NAI-HKNJANUMLEN     PIC 9(05).
                       09  SPA-NAI-HKNNUM-BF       PIC X(03).
      *
               05  SPA-MOT-AREA.
                   07  SPA-MOT-KBNCD           PIC X(08).
                   07  SPA-MOT-STYUKYMD        PIC X(08).
                   07  SPA-MOT-EDYUKYMD        PIC X(08).
                   07  SPA-MOT-HKNJANUM        PIC X(03).
                   07  SPA-MOT-JYKNKBN         PIC X(01).
                   07  SPA-MOT-HKNNUM          PIC X(03).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-CHK004          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-DABURI          PIC 9(01).
           03  FLG-SKIP            PIC 9(01).
           03  FLG-CLIST-ROW       PIC 9(01).
           03  FLG-HIZUKE          PIC 9(01).
           03  FLG-KANACHK         PIC S9(9)   BINARY.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX-CMB             PIC 9(05).
           03  IDX-LST             PIC 9(05).
           03  IDX-KGNM            PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG          PIC X(70).
      *
           03  WRK-YMD             PIC X(10).
           03  WRK-SYMD            PIC X(08).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-STYUKYMD        PIC X(08).
           03  WRK-EDYUKYMD        PIC X(08).
      *
           03  WRK-UPDKBN          PIC X(03).
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-CMB-CD          PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM     PIC X(200).
           03  WRK-HKNJANUM        PIC X(08).
           03  WRK-KANACHK-DATA    PIC X(1000).
           03  WRK-KANACHK-MAXLEN  PIC 9(05).
           03  WRK-KBNCD-G.
               05  WRK-KBNCD       PIC 9(06).
               05  WRK-KBNCDFL     PIC X(02).
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-LST             PIC 9(05).
      *
       01  WRK-ROW.
           05  WRK-CLIST-ROW       PIC S9(9)   BINARY.
      *
       01  CONST-AREA.
           03  CONST-CLIST-MAX     PIC 9(05)   VALUE   200.
           03  CONST-INSERT        PIC X(03)   VALUE   "INS".
           03  CONST-UPDATE        PIC X(03)   VALUE   "UPD".
           03  CONST-DELETE        PIC X(03)   VALUE   "DEL".
           03  CONST-H039          PIC X(03)   VALUE   "039".
           03  CONST-H009          PIC X(03)   VALUE   "009".
      *
           COPY    "CPKGNMCHK.INC".
      *
           COPY    "CPCHK004.INC"  REPLACING   //CHK004-//
                                   BY          //WKCHK004-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    チェック排除情報
           COPY  "CPSK1040.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    チェックマスタ４
           COPY    "CPCHK004.INC".
      *
           COPY    "CPCK1102.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    文字妥当性チェック
           COPY    "CPORCXKANACONV.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE               DIVISION    USING
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W63
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"      ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"      ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END             =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W63         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "WERR" )
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END             =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
      *            エラーメッセージ表示処理
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "W63    "       TO  MCP-WINDOW
      *
      *    ＰＵＴ　処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "WID1"
      *        確認画面ＯＫ処理
               PERFORM 330-WID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＷＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "3001"
      *            削除処理
                   PERFORM 481-SAKUJO-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-W63
      *
           PERFORM 420-CLEAR-SEC
      *
      *    チェック排除情報リスト編集処理
           PERFORM 3101-CLIST-SET-SEC
           MOVE    1               TO  FLG-CLIST-ROW
      *
      *    カーソル位置をセット
           IF    ( SPA-GMN-CLIST-MAX   >   ZERO )
               MOVE    001             TO  SPA-GMN-CUR
           ELSE
               MOVE    002             TO  SPA-GMN-CUR
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボ設定処理
      *****************************************************************
       3101-COMBO-SET-SEC          SECTION.
      *
           PERFORM 31011-JYKNKBNLST-SET-SEC
      *
           PERFORM 31011-HKNNUMLST-SET-SEC
      *
           PERFORM 31011-KGCHKPTNLST-SET-SEC
      *
           PERFORM 31011-KGSUJILST-SET-SEC
      *
           PERFORM 31011-KGKANJILST-SET-SEC
      *
           PERFORM 31011-KGABTLST-SET-SEC
      *
           PERFORM 31011-KGHYPNLST-SET-SEC
      *
           PERFORM 31011-KGKAKKOLST-SET-SEC
      *
           PERFORM 31011-KGETCLST-SET-SEC
      *
           PERFORM 31011-KGKETACHKLST-SET-SEC
      *
           PERFORM 31011-KGKETAMINLST-SET-SEC
      *
           PERFORM 31011-KGKANJINASHILST-SET-SEC
      *
           PERFORM 31011-NMSUJILST-SET-SEC
      *
           PERFORM 31011-NMKANJILST-SET-SEC
      *
           PERFORM 31011-NMABTLST-SET-SEC
      *
           PERFORM 31011-NMHYPNLST-SET-SEC
      *
           PERFORM 31011-NMKAKKOLST-SET-SEC
      *
           PERFORM 31011-NMETCLST-SET-SEC
      *
           PERFORM 31011-NMSUJINASHILST-SET-SEC
      *
           PERFORM 31011-NMKETACHKLST-SET-SEC
      *
           PERFORM 31011-NMKETAMINLST-SET-SEC
      *
           .
       3101-COMBO-SET-EXT.
           EXIT.
      *****************************************************************
      *    一致条件コンボ設定処理
      *****************************************************************
       31011-JYKNKBNLST-SET-SEC        SECTION.
      *
           INITIALIZE              SPA-GMN-JYKNKBNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-JYKNKBNL(1)
           MOVE    "1"             TO  SPA-GMN-JYKNKBNL(2)
           MOVE    "2"             TO  SPA-GMN-JYKNKBNL(3)
           MOVE    "完全一致"      TO  SPA-GMN-JYKNKBNMEIL(1)
           MOVE    "前方一致"      TO  SPA-GMN-JYKNKBNMEIL(2)
           MOVE    "指定なし"      TO  SPA-GMN-JYKNKBNMEIL(3)
      *
           MOVE    3               TO  SPA-GMN-JYKNKBN-MAX
      *
           .
       31011-JYKNKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類コンボ設定処理
      *****************************************************************
       31011-HKNNUMLST-SET-SEC         SECTION.
      *
           INITIALIZE              SPA-GMN-HKNNUMLST-G
      *
           PERFORM 900-HKNNUM-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-HKNNUM  NOT =   ZERO )
                    OR   ( SPA-GMN-HKNNUM-MAX
                                          >=   50   )
               IF    ( HKN-HKNKOHSBTKBN    =   "1" OR  "4"
                                           OR  "8" OR  "9" )
                   MOVE    SPA-GMN-HKNNUM-MAX
                                           TO  IDX-CMB
      *            保険
                   IF    ( IDX-CMB         >   ZERO )
                       IF    ( HKN-HKNNUM  NOT =   SPA-GMN-HKNNUML
                                                             (IDX-CMB))
      *                    保険情報編集
                           PERFORM 310111-HKNNUM-SET-SEC
                       END-IF
                   ELSE
      *                保険情報編集
                       PERFORM 310111-HKNNUM-SET-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-HKNNUM-KEY3-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_hknnum"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       31011-HKNNUMLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類設定処理
      *****************************************************************
       310111-HKNNUM-SET-SEC           SECTION.
      *
           COMPUTE SPA-GMN-HKNNUM-MAX  =   SPA-GMN-HKNNUM-MAX  +   1
      *
           MOVE    SPA-GMN-HKNNUM-MAX  TO  IDX-CMB
      *
           MOVE    HKN-HKNNUM          TO  SPA-GMN-HKNNUML   (IDX-CMB)
           MOVE    HKN-TANSEIDONAME    TO  SPA-GMN-HKNNUMMEIL(IDX-CMB)
      *
           .
       310111-HKNNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    記号抜けコンボボックス設定処理
      *****************************************************************
       31011-KGCHKPTNLST-SET-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-KGCHKPTNLST-G
      *
           MOVE    "0"             TO  SPA-GMN-KGCHKPTNL(1)
           MOVE    "1"             TO  SPA-GMN-KGCHKPTNL(2)
           MOVE    "2"             TO  SPA-GMN-KGCHKPTNL(3)
           MOVE    "チェックする"
                                   TO  SPA-GMN-KGCHKPTNMEIL(1)
           MOVE    "チェックしない"
                                   TO  SPA-GMN-KGCHKPTNMEIL(2)
           MOVE    "チェックする（入力がある場合はエラー）"
                                   TO  SPA-GMN-KGCHKPTNMEIL(3)
      *
           MOVE    3               TO  SPA-GMN-KGCHKPTN-MAX
      *
           .
       31011-KGCHKPTNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    記号数字チェックコンボボックス設定処理
      *****************************************************************
       31011-KGSUJILST-SET-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-KGSUJILST-G
      *
           MOVE    "1"             TO  SPA-GMN-KGSUJIL(1)
           MOVE    "2"             TO  SPA-GMN-KGSUJIL(2)
           MOVE    "認める"        TO  SPA-GMN-KGSUJIMEIL(1)
           MOVE    "エラーとする"  TO  SPA-GMN-KGSUJIMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-KGSUJI-MAX
      *
           .
       31011-KGSUJILST-SET-EXT.
           EXIT.
      *****************************************************************
      *    記号漢字チェックコンボボックス設定処理
      *****************************************************************
       31011-KGKANJILST-SET-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-KGKANJILST-G
      *
           MOVE    "1"             TO  SPA-GMN-KGKANJIL(1)
           MOVE    "2"             TO  SPA-GMN-KGKANJIL(2)
           MOVE    "認める"        TO  SPA-GMN-KGKANJIMEIL(1)
           MOVE    "エラーとする"  TO  SPA-GMN-KGKANJIMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-KGKANJI-MAX
      *
           .
       31011-KGKANJILST-SET-EXT.
           EXIT.
      *****************************************************************
      *    記号英字チェックコンボボックス設定処理
      *****************************************************************
       31011-KGABTLST-SET-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-KGABTLST-G
      *
           MOVE    "1"             TO  SPA-GMN-KGABTL(1)
           MOVE    "2"             TO  SPA-GMN-KGABTL(2)
           MOVE    "認める"        TO  SPA-GMN-KGABTMEIL(1)
           MOVE    "エラーとする"  TO  SPA-GMN-KGABTMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-KGABT-MAX
      *
           .
       31011-KGABTLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    記号ハイフンチェックコンボボックス設定処理
      *****************************************************************
       31011-KGHYPNLST-SET-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-KGHYPNLST-G
      *
           MOVE    "1"             TO  SPA-GMN-KGHYPNL(1)
           MOVE    "2"             TO  SPA-GMN-KGHYPNL(2)
           MOVE    "認める"        TO  SPA-GMN-KGHYPNMEIL(1)
           MOVE    "エラーとする"  TO  SPA-GMN-KGHYPNMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-KGHYPN-MAX
      *
           .
       31011-KGHYPNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    記号括弧コンボボックス設定処理
      *****************************************************************
       31011-KGKAKKOLST-SET-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-KGKAKKOLST-G
      *
           MOVE    "1"             TO  SPA-GMN-KGKAKKOL(1)
           MOVE    "2"             TO  SPA-GMN-KGKAKKOL(2)
           MOVE    "認める"        TO  SPA-GMN-KGKAKKOMEIL(1)
           MOVE    "エラーとする"  TO  SPA-GMN-KGKAKKOMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-KGKAKKO-MAX
      *
           .
       31011-KGKAKKOLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    記号その他コンボボックス設定処理
      *****************************************************************
       31011-KGETCLST-SET-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-KGETCLST-G
      *
           MOVE    "1"             TO  SPA-GMN-KGETCL(1)
           MOVE    "2"             TO  SPA-GMN-KGETCL(2)
           MOVE    "認める"        TO  SPA-GMN-KGETCMEIL(1)
           MOVE    "エラーとする"  TO  SPA-GMN-KGETCMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-KGETC-MAX
      *
           .
       31011-KGETCLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    記号漢字無しコンボボックス設定処理
      *****************************************************************
       31011-KGKANJINASHILST-SET-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-KGKANJINASHILST-G
      *
           MOVE    "1"             TO  SPA-GMN-KGKANJINASHIL(1)
           MOVE    "2"             TO  SPA-GMN-KGKANJINASHIL(2)
           MOVE    "認める"        TO  SPA-GMN-KGKANJINASHIMEIL(1)
           MOVE    "エラーとする"  TO  SPA-GMN-KGKANJINASHIMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-KGKANJINASHI-MAX
      *
           .
       31011-KGKANJINASHILST-SET-EXT.
           EXIT.
      *****************************************************************
      *    記号桁数チェックコンボボックス設定処理
      *****************************************************************
       31011-KGKETACHKLST-SET-SEC    SECTION.
      *
           INITIALIZE                  SPA-GMN-KGKETACHKLST-G
      *
           MOVE    "0"             TO  SPA-GMN-KGKETACHKL(1)
           MOVE    "チェックしない" TO SPA-GMN-KGKETACHKMEIL(1)
      *
           MOVE    "1"             TO  SPA-GMN-KGKETACHKL(2)
           MOVE    "チェックする"
                                   TO  SPA-GMN-KGKETACHKMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-KGKETACHK-MAX
      *
           .
       31011-KGKETACHKLST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    記号桁数下限コンボボックス設定処理
      *****************************************************************
       31011-KGKETAMINLST-SET-SEC    SECTION.
      *
           INITIALIZE                  SPA-GMN-KGKETAMINLST-G
      *
           MOVE    "01"            TO  SPA-GMN-KGKETAMINL(1)
           MOVE    "１桁"          TO  SPA-GMN-KGKETAMINMEIL(1)
      *
           MOVE    "02"            TO  SPA-GMN-KGKETAMINL(2)
           MOVE    "２桁"          TO  SPA-GMN-KGKETAMINMEIL(2)
      *
           MOVE    "03"            TO  SPA-GMN-KGKETAMINL(3)
           MOVE    "３桁"          TO  SPA-GMN-KGKETAMINMEIL(3)
      *
           MOVE    "04"            TO  SPA-GMN-KGKETAMINL(4)
           MOVE    "４桁"          TO  SPA-GMN-KGKETAMINMEIL(4)
      *
           MOVE    "05"            TO  SPA-GMN-KGKETAMINL(5)
           MOVE    "５桁"          TO  SPA-GMN-KGKETAMINMEIL(5)
      *
           MOVE    "06"            TO  SPA-GMN-KGKETAMINL(6)
           MOVE    "６桁"          TO  SPA-GMN-KGKETAMINMEIL(6)
      *
           MOVE    "07"            TO  SPA-GMN-KGKETAMINL(7)
           MOVE    "７桁"          TO  SPA-GMN-KGKETAMINMEIL(7)
      *
           MOVE    "08"            TO  SPA-GMN-KGKETAMINL(8)
           MOVE    "８桁"          TO  SPA-GMN-KGKETAMINMEIL(8)
      *
           MOVE    "09"            TO  SPA-GMN-KGKETAMINL(9)
           MOVE    "９桁"          TO  SPA-GMN-KGKETAMINMEIL(9)
      *
           MOVE    "10"            TO  SPA-GMN-KGKETAMINL(10)
           MOVE    "１０桁"        TO  SPA-GMN-KGKETAMINMEIL(10)
      *
           MOVE    "11"            TO  SPA-GMN-KGKETAMINL(11)
           MOVE    "１１桁"        TO  SPA-GMN-KGKETAMINMEIL(11)
      *
           MOVE    "12"            TO  SPA-GMN-KGKETAMINL(12)
           MOVE    "１２桁"        TO  SPA-GMN-KGKETAMINMEIL(12)
      *
           MOVE    "13"            TO  SPA-GMN-KGKETAMINL(13)
           MOVE    "１３桁"        TO  SPA-GMN-KGKETAMINMEIL(13)
      *
           MOVE    "14"            TO  SPA-GMN-KGKETAMINL(14)
           MOVE    "１４桁"        TO  SPA-GMN-KGKETAMINMEIL(14)
      *
           MOVE    "15"            TO  SPA-GMN-KGKETAMINL(15)
           MOVE    "１５桁"        TO  SPA-GMN-KGKETAMINMEIL(15)
      *
           MOVE    "16"            TO  SPA-GMN-KGKETAMINL(16)
           MOVE    "１６桁"        TO  SPA-GMN-KGKETAMINMEIL(16)
      *
           MOVE    "17"            TO  SPA-GMN-KGKETAMINL(17)
           MOVE    "１７桁"        TO  SPA-GMN-KGKETAMINMEIL(17)
      *
           MOVE    "18"            TO  SPA-GMN-KGKETAMINL(18)
           MOVE    "１８桁"        TO  SPA-GMN-KGKETAMINMEIL(18)
      *
           MOVE    "19"            TO  SPA-GMN-KGKETAMINL(19)
           MOVE    "１９桁"        TO  SPA-GMN-KGKETAMINMEIL(19)
      *
           MOVE    "20"            TO  SPA-GMN-KGKETAMINL(20)
           MOVE    "２０桁"        TO  SPA-GMN-KGKETAMINMEIL(20)
      *
           MOVE    20              TO  SPA-GMN-KGKETAMIN-MAX
      *
           .
       31011-KGKETAMINLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    番号数字チェックコンボボックス設定処理
      *****************************************************************
       31011-NMSUJILST-SET-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-NMSUJILST-G
      *
           MOVE    "1"             TO  SPA-GMN-NMSUJIL(1)
           MOVE    "認める"        TO  SPA-GMN-NMSUJIMEIL(1)
      *
           MOVE    1               TO  SPA-GMN-NMSUJI-MAX
      *
           .
       31011-NMSUJILST-SET-EXT.
           EXIT.
      *****************************************************************
      *    番号漢字チェックコンボボックス設定処理
      *****************************************************************
       31011-NMKANJILST-SET-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-NMKANJILST-G
      *
           MOVE    "1"             TO  SPA-GMN-NMKANJIL(1)
           MOVE    "2"             TO  SPA-GMN-NMKANJIL(2)
           MOVE    "認める"        TO  SPA-GMN-NMKANJIMEIL(1)
           MOVE    "エラーとする"  TO  SPA-GMN-NMKANJIMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-NMKANJI-MAX
      *
           .
       31011-NMKANJILST-SET-EXT.
           EXIT.
      *****************************************************************
      *    番号英字チェックコンボボックス設定処理
      *****************************************************************
       31011-NMABTLST-SET-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-NMABTLST-G
      *
           MOVE    "1"             TO  SPA-GMN-NMABTL(1)
           MOVE    "2"             TO  SPA-GMN-NMABTL(2)
           MOVE    "認める"        TO  SPA-GMN-NMABTMEIL(1)
           MOVE    "エラーとする"  TO  SPA-GMN-NMABTMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-NMABT-MAX
      *
           .
       31011-NMABTLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    番号ハイフンチェックコンボボックス設定処理
      *****************************************************************
       31011-NMHYPNLST-SET-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-NMHYPNLST-G
      *
           MOVE    "1"             TO  SPA-GMN-NMHYPNL(1)
           MOVE    "2"             TO  SPA-GMN-NMHYPNL(2)
           MOVE    "認める"        TO  SPA-GMN-NMHYPNMEIL(1)
           MOVE    "エラーとする"  TO  SPA-GMN-NMHYPNMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-NMHYPN-MAX
      *
           .
       31011-NMHYPNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    番号括弧コンボボックス設定処理
      *****************************************************************
       31011-NMKAKKOLST-SET-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-NMKAKKOLST-G
      *
           MOVE    "1"             TO  SPA-GMN-NMKAKKOL(1)
           MOVE    "2"             TO  SPA-GMN-NMKAKKOL(2)
           MOVE    "認める"        TO  SPA-GMN-NMKAKKOMEIL(1)
           MOVE    "エラーとする"  TO  SPA-GMN-NMKAKKOMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-NMKAKKO-MAX
      *
           .
       31011-NMKAKKOLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    番号その他コンボボックス設定処理
      *****************************************************************
       31011-NMETCLST-SET-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-NMETCLST-G
      *
           MOVE    "1"             TO  SPA-GMN-NMETCL(1)
           MOVE    "2"             TO  SPA-GMN-NMETCL(2)
           MOVE    "認める"        TO  SPA-GMN-NMETCMEIL(1)
           MOVE    "エラーとする"  TO  SPA-GMN-NMETCMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-NMETC-MAX
      *
           .
       31011-NMETCLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    番号数字無しコンボボックス設定処理
      *****************************************************************
       31011-NMSUJINASHILST-SET-SEC    SECTION.
      *
           INITIALIZE              SPA-GMN-NMSUJINASHILST-G
      *
           MOVE    "2"             TO  SPA-GMN-NMSUJINASHIL(1)
           MOVE    "エラーとする"  TO  SPA-GMN-NMSUJINASHIMEIL(1)
      *
           MOVE    1               TO  SPA-GMN-NMSUJINASHI-MAX
      *
           .
       31011-NMSUJINASHILST-SET-EXT.
           EXIT.
      *****************************************************************
      *    番号桁数チェックコンボボックス設定処理
      *****************************************************************
       31011-NMKETACHKLST-SET-SEC    SECTION.
      *
           INITIALIZE                  SPA-GMN-NMKETACHKLST-G
      *
           MOVE    "0"             TO  SPA-GMN-NMKETACHKL(1)
           MOVE    "チェックしない" TO SPA-GMN-NMKETACHKMEIL(1)
      *
           MOVE    "1"             TO  SPA-GMN-NMKETACHKL(2)
           MOVE    "チェックする"
                                   TO  SPA-GMN-NMKETACHKMEIL(2)
      *
           MOVE    2               TO  SPA-GMN-NMKETACHK-MAX
      *
           .
       31011-NMKETACHKLST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号桁数下限コンボボックス設定処理
      *****************************************************************
       31011-NMKETAMINLST-SET-SEC    SECTION.
      *
           INITIALIZE                  SPA-GMN-NMKETAMINLST-G
      *
           MOVE    "01"            TO  SPA-GMN-NMKETAMINL(1)
           MOVE    "１桁"          TO  SPA-GMN-NMKETAMINMEIL(1)
      *
           MOVE    "02"            TO  SPA-GMN-NMKETAMINL(2)
           MOVE    "２桁"          TO  SPA-GMN-NMKETAMINMEIL(2)
      *
           MOVE    "03"            TO  SPA-GMN-NMKETAMINL(3)
           MOVE    "３桁"          TO  SPA-GMN-NMKETAMINMEIL(3)
      *
           MOVE    "04"            TO  SPA-GMN-NMKETAMINL(4)
           MOVE    "４桁"          TO  SPA-GMN-NMKETAMINMEIL(4)
      *
           MOVE    "05"            TO  SPA-GMN-NMKETAMINL(5)
           MOVE    "５桁"          TO  SPA-GMN-NMKETAMINMEIL(5)
      *
           MOVE    "06"            TO  SPA-GMN-NMKETAMINL(6)
           MOVE    "６桁"          TO  SPA-GMN-NMKETAMINMEIL(6)
      *
           MOVE    "07"            TO  SPA-GMN-NMKETAMINL(7)
           MOVE    "７桁"          TO  SPA-GMN-NMKETAMINMEIL(7)
      *
           MOVE    "08"            TO  SPA-GMN-NMKETAMINL(8)
           MOVE    "８桁"          TO  SPA-GMN-NMKETAMINMEIL(8)
      *
           MOVE    "09"            TO  SPA-GMN-NMKETAMINL(9)
           MOVE    "９桁"          TO  SPA-GMN-NMKETAMINMEIL(9)
      *
           MOVE    "10"            TO  SPA-GMN-NMKETAMINL(10)
           MOVE    "１０桁"        TO  SPA-GMN-NMKETAMINMEIL(10)
      *
           MOVE    10              TO  SPA-GMN-NMKETAMIN-MAX
      *
           .
       31011-NMKETAMINLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    チェック排除情報リスト編集処理
      *****************************************************************
       3101-CLIST-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-CLIST-AREA
                                       SPA-NAI-CLIST-AREA
      *
           INITIALIZE                  CK4-1102-REC
           MOVE    "1102"          TO  CK4-1102-KANRICD
           MOVE    CK4-1102-REC    TO  CHK004-REC
           PERFORM 900-CHK004-KEY3-SEL-SEC
           MOVE    CHK004-REC       TO  CK4-1102-REC
      *
           MOVE    ZERO            TO  SPA-GMN-CLIST-MAX
           PERFORM UNTIL ( FLG-CHK004       =   1  )
                   OR    ( IDX-LST        >=  CONST-CLIST-MAX )
      *
               COMPUTE SPA-GMN-CLIST-MAX
                   =   SPA-GMN-CLIST-MAX   +  1
      *
               MOVE    SPA-GMN-CLIST-MAX
                                   TO  IDX-LST
      *
               MOVE    IDX-LST     TO  SPA-GMN-TNUM(IDX-LST)
      *
               MOVE    CK4-1102-HKNJANUM
                                   TO  SPA-GMN-THKNJANUM(IDX-LST)
               EVALUATE    CK4-1102-JYKNKBN
               WHEN    "0"
                   MOVE    "完全"      TO  SPA-GMN-TJYKNKBN(IDX-LST)
               WHEN    "1"
                   MOVE    "前方"      TO  SPA-GMN-TJYKNKBN(IDX-LST)
               WHEN    "2"
                   MOVE    "　　"      TO  SPA-GMN-TJYKNKBN(IDX-LST)
               END-EVALUATE
               MOVE    CK4-1102-JYKNKBN
                                   TO  SPA-NAI-TJYKNKBN(IDX-LST)
      *
               IF    ( CK4-1102-JYKNKBN    =   "0" )
                   MOVE    CK4-1102-HKNJANUM
                                   TO  WRK-HKNJANUM
                   PERFORM 900-HKNJAINF-KEY-SEL-SEC
                   MOVE    HKNJA-HKNJANAME
                                   TO  SPA-GMN-THKNJANAME(IDX-LST)
               END-IF
      *
               PERFORM 900-HKNNUM-KEY-SEL-SEC
               MOVE    HKN-TANSEIDONAME
                                   TO  SPA-GMN-THKNNUM   (IDX-LST)
               MOVE    CK4-1102-HKNNUM
                                   TO  SPA-NAI-THKNNUM   (IDX-LST)
      *
               IF    ( CK4-1102-KIGOCHK    =   "0" )
                   MOVE    "　○"    TO  SPA-GMN-TKIGOCHK  (IDX-LST)
               ELSE
                   MOVE    "　−"    TO  SPA-GMN-TKIGOCHK  (IDX-LST)
               END-IF
      *
               MOVE    CK4-1102-STYUKYMD   TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TSTYUKYMD(IDX-LST)
               MOVE    WRK-SYMD        TO  SPA-NAI-TSTYUKYMD(IDX-LST)
      *
               MOVE    CK4-1102-EDYUKYMD   TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-TEDYUKYMD(IDX-LST)
               MOVE    WRK-SYMD        TO  SPA-NAI-TEDYUKYMD(IDX-LST)
      *
               MOVE    CK4-1102-KBNCD  TO  SPA-NAI-TKBNCD   (IDX-LST)
      *
               PERFORM 900-CHK004-KEY3-FET-SEC
               MOVE    CHK004-REC       TO  CK4-1102-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-CLIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 480-TUIKA-SEC
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 480-HENKOU-SEC
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 480-SAKUJO-SEC
           WHEN    "CLICKED"       ALSO    "UP"
               PERFORM 480-CHECKLIST-ITEM-UP-SEC
           WHEN    "CLICKED"       ALSO    "DOWN"
               PERFORM 480-CHECKLIST-ITEM-DOWN-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"  ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    ANY         ALSO    "CHECKLIST"
               PERFORM 4100-CLIST-SET-SEC
           WHEN    OTHER
               MOVE    ZERO        TO  SPA-GMN-CUR
               PERFORM 4101-INPUT-SET-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号編集処理
      *****************************************************************
       4100-SELNUM-CHK-SEC         SECTION.
      *
           MOVE    W63-SELNUM      TO  SPA-GMN-SELNUM
      *
           IF    ( SPA-GMN-SELNUM      =   ZERO )
               PERFORM 420-CLEAR-SEC
           ELSE
               PERFORM 4101-SELNUM-CHK-SEC
               IF    ( SPA-ERRCD       =  SPACE )
                   MOVE    SPA-GMN-SELNUM  TO  IDX-LST
                   PERFORM 420-INPUT-LIST-SET-SEC
               END-IF
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           .
      *
       4100-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックリスト編集処理
      *****************************************************************
       4100-CLIST-SET-SEC          SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-SELNUM
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST     >       SPA-GMN-CLIST-MAX )
                    OR   ( SPA-GMN-SELNUM    NOT   =   ZERO )
      *
               IF    ( W63-CHECKLIST-SELECT(IDX-LST)   =   "T" )
                   PERFORM 420-INPUT-LIST-SET-SEC
                   MOVE    IDX-LST         TO  SPA-GMN-SELNUM
               END-IF
           END-PERFORM
      *
           .
      *
       4100-CLIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    リストの内容をセット処理
      *****************************************************************
       420-INPUT-LIST-SET-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-INPUT-AREA
                                       SPA-NAI-INPUT-AREA
                                       SPA-MOT-AREA
      *
           INITIALIZE                  CK4-1102-REC
           MOVE    "1102"          TO  CK4-1102-KANRICD
           MOVE    SPA-NAI-TKBNCD   (IDX-LST)
                                   TO  CK4-1102-KBNCD
           MOVE    SPA-NAI-TSTYUKYMD(IDX-LST)
                                   TO  CK4-1102-STYUKYMD
                                       CK4-1102-EDYUKYMD
           MOVE    CK4-1102-REC    TO  CHK004-REC
           PERFORM 900-CHK004-KEY-SEL-SEC
           MOVE    CHK004-REC       TO  CK4-1102-REC
      *
           MOVE    CK4-1102-STYUKYMD
                                   TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-STYUKYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-STYUKYMD
      *
           MOVE    CK4-1102-EDYUKYMD
                                   TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-EDYUKYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-EDYUKYMD
      *
           PERFORM 31011-HKNNUMLST-SET-SEC
      *
           MOVE    CK4-1102-HKNJANUM
                                   TO  SPA-GMN-HKNJANUM
           MOVE    CK4-1102-JYKNKBN
                                   TO  WRK-CMB-CD
           PERFORM 4200-JYKNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JYKNKBN-G
      *
           IF    ( SPA-GMN-JYKNKBN =   "0" )
               MOVE    SPA-GMN-HKNJANUM
                                   TO  WRK-HKNJANUM
               PERFORM 900-HKNJAINF-KEY-SEL-SEC
               MOVE    HKNJA-HKNJANAME
                                   TO  SPA-GMN-HKNJANAME
           END-IF
      *
           MOVE    CK4-1102-HKNNUM TO  WRK-CMB-CD
           PERFORM 4200-HKNNUMLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-HKNNUM-G
           MOVE    SPA-GMN-HKNNUM  TO  SPA-NAI-HKNNUM-BF
      *
           IF    ( CK4-1102-UPDDUMMY   =   SPACE )
      *
      *
               MOVE    CK4-1102-KIGOCHK      TO  WRK-CMB-CD
               PERFORM 4200-KGCHKPTNLST-CHK-SEC
               MOVE    WRK-CMB-ITM           TO  SPA-GMN-KGCHKPTN-G
      *
               PERFORM 4201-CHKCMB-DEFAULT-SET-SEC
               PERFORM 4202-CHKCMB-DEFAULT-SET-SEC
           ELSE
               MOVE    CK4-1102-KGCHKPTN     TO  WRK-CMB-CD
               PERFORM 4200-KGCHKPTNLST-CHK-SEC
               MOVE    WRK-CMB-ITM           TO  SPA-GMN-KGCHKPTN-G
      *
               MOVE    CK4-1102-KGSUJI       TO  SPA-GMN-KGSUJI
               MOVE    CK4-1102-KGKANJI      TO  SPA-GMN-KGKANJI
               MOVE    CK4-1102-KGABT        TO  SPA-GMN-KGABT
               MOVE    CK4-1102-KGHYPN       TO  SPA-GMN-KGHYPN
               MOVE    CK4-1102-KGKAKKO      TO  SPA-GMN-KGKAKKO
               MOVE    CK4-1102-KGETC        TO  SPA-GMN-KGETC
               MOVE    CK4-1102-KGKANJINASHI TO  SPA-GMN-KGKANJINASHI
               MOVE    CK4-1102-KGKETAMIN    TO  SPA-GMN-KGKETAMIN
               IF    ( SPA-GMN-KGKETAMIN     =   SPACE )
                   MOVE    "0"               TO  SPA-GMN-KGKETACHK
               ELSE
                   MOVE    "1"               TO  SPA-GMN-KGKETACHK
               END-IF
      *
               MOVE    CK4-1102-NM1SUJI      TO  SPA-GMN-NM1SUJI
               MOVE    CK4-1102-NM1KANJI     TO  SPA-GMN-NM1KANJI
               MOVE    CK4-1102-NM1ABT       TO  SPA-GMN-NM1ABT
               MOVE    CK4-1102-NM1HYPN      TO  SPA-GMN-NM1HYPN
               MOVE    CK4-1102-NM1KAKKO     TO  SPA-GMN-NM1KAKKO
               MOVE    CK4-1102-NM1ETC       TO  SPA-GMN-NM1ETC
               MOVE    CK4-1102-NMSUJINASHI  TO  SPA-GMN-NMSUJINASHI
               MOVE    CK4-1102-NMKETAMIN    TO  SPA-GMN-NMKETAMIN
               IF    ( SPA-GMN-NMKETAMIN     =   SPACE )
                   MOVE    "0"               TO  SPA-GMN-NMKETACHK
               ELSE
                   MOVE    "1"               TO  SPA-GMN-NMKETACHK
               END-IF
      *
               MOVE    CK4-1102-NM2SUJI      TO  SPA-GMN-NM2SUJI
               MOVE    CK4-1102-NM2KANJI     TO  SPA-GMN-NM2KANJI
               MOVE    CK4-1102-NM2ABT       TO  SPA-GMN-NM2ABT
               MOVE    CK4-1102-NM2HYPN      TO  SPA-GMN-NM2HYPN
               MOVE    CK4-1102-NM2KAKKO     TO  SPA-GMN-NM2KAKKO
               MOVE    CK4-1102-NM2ETC       TO  SPA-GMN-NM2ETC
      *
               PERFORM 4202-CHKCMB-DEFAULT-SET-SEC
           END-IF
      *
           MOVE    CK4-1102-KBNCD  TO  SPA-MOT-KBNCD
           MOVE    CK4-1102-STYUKYMD
                                   TO  SPA-MOT-STYUKYMD
           MOVE    CK4-1102-EDYUKYMD
                                   TO  SPA-MOT-EDYUKYMD
      *
           MOVE    CK4-1102-HKNJANUM
                                   TO  SPA-MOT-HKNJANUM
           MOVE    CK4-1102-JYKNKBN
                                   TO  SPA-MOT-JYKNKBN
           MOVE    CK4-1102-HKNNUM
                                   TO  SPA-MOT-HKNNUM
      *
           .
       420-INPUT-LIST-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックコンボボックス初期値設定処理
      *****************************************************************
       4201-CHKCMB-DEFAULT-SET-SEC     SECTION.
      *
           MOVE    KGNMCHK-HDEF-IDX        TO  IDX-KGNM
           EVALUATE    SPA-GMN-HKNNUM
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
      *
           MOVE    KGNMCHK-KGSUJI(IDX-KGNM)
                                       TO  SPA-GMN-KGSUJI
           MOVE    KGNMCHK-KGKANJI (IDX-KGNM)
                                       TO  SPA-GMN-KGKANJI
           MOVE    KGNMCHK-KGABT(IDX-KGNM)
                                       TO  SPA-GMN-KGABT
           MOVE    KGNMCHK-KGHYPN(IDX-KGNM)
                                       TO  SPA-GMN-KGHYPN
           MOVE    KGNMCHK-KGKAKKO(IDX-KGNM)
                                       TO  SPA-GMN-KGKAKKO
           MOVE    KGNMCHK-KGETC(IDX-KGNM)
                                       TO  SPA-GMN-KGETC
           MOVE    KGNMCHK-KGKANJINASHI(IDX-KGNM)
                                       TO  SPA-GMN-KGKANJINASHI
           MOVE    KGNMCHK-KGKETAMIN  (IDX-KGNM)
                                       TO  SPA-GMN-KGKETAMIN
           IF    ( SPA-GMN-KGKETAMIN   =   SPACE )
               MOVE    "0"             TO  SPA-GMN-KGKETACHK
           ELSE
               MOVE    "1"             TO  SPA-GMN-KGKETACHK
           END-IF
      *
           MOVE    KGNMCHK-NM1SUJI(IDX-KGNM)
                                       TO  SPA-GMN-NM1SUJI
           MOVE    KGNMCHK-NM1KANJI(IDX-KGNM)
                                       TO  SPA-GMN-NM1KANJI
           MOVE    KGNMCHK-NM1ABT(IDX-KGNM)
                                       TO  SPA-GMN-NM1ABT
           MOVE    KGNMCHK-NM1HYPN(IDX-KGNM)
                                       TO  SPA-GMN-NM1HYPN
           MOVE    KGNMCHK-NM1KAKKO(IDX-KGNM)
                                       TO  SPA-GMN-NM1KAKKO
           MOVE    KGNMCHK-NM1ETC(IDX-KGNM)
                                       TO  SPA-GMN-NM1ETC
           MOVE    KGNMCHK-NMSUJINASHI(IDX-KGNM)
                                       TO  SPA-GMN-NMSUJINASHI
           MOVE    KGNMCHK-NMKETAMIN  (IDX-KGNM)
                                       TO  SPA-GMN-NMKETAMIN
           IF    ( SPA-GMN-NMKETAMIN   =   SPACE )
               MOVE    "0"             TO  SPA-GMN-NMKETACHK
           ELSE
               MOVE    "1"             TO  SPA-GMN-NMKETACHK
           END-IF
      *
           MOVE    KGNMCHK-NM2SUJI(IDX-KGNM)
                                       TO  SPA-GMN-NM2SUJI
           MOVE    KGNMCHK-NM2KANJI(IDX-KGNM)
                                       TO  SPA-GMN-NM2KANJI
           MOVE    KGNMCHK-NM2ABT(IDX-KGNM)
                                       TO  SPA-GMN-NM2ABT
           MOVE    KGNMCHK-NM2HYPN(IDX-KGNM)
                                       TO  SPA-GMN-NM2HYPN
           MOVE    KGNMCHK-NM2KAKKO(IDX-KGNM)
                                       TO  SPA-GMN-NM2KAKKO
           MOVE    KGNMCHK-NM2ETC(IDX-KGNM)
                                       TO  SPA-GMN-NM2ETC
      *
           .
       4201-CHKCMB-DEFAULT-SET-EXT.
           EXIT.
      *****************************************************************
      *    チェックコンボボックス初期値設定処理
      *****************************************************************
       4202-CHKCMB-DEFAULT-SET-SEC     SECTION.
      *
           MOVE    SPA-GMN-KGSUJI  TO  WRK-CMB-CD
           PERFORM 4200-KGSUJILST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KGSUJI-G
      *
           MOVE    SPA-GMN-KGKANJI TO  WRK-CMB-CD
           PERFORM 4200-KGKANJILST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KGKANJI-G
      *
           MOVE    SPA-GMN-KGABT   TO  WRK-CMB-CD
           PERFORM 4200-KGABTLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KGABT-G
      *
           MOVE    SPA-GMN-KGHYPN  TO  WRK-CMB-CD
           PERFORM 4200-KGHYPNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KGHYPN-G
      *
           MOVE    SPA-GMN-KGKAKKO TO  WRK-CMB-CD
           PERFORM 4200-KGKAKKOLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KGKAKKO-G
      *
           MOVE    SPA-GMN-KGETC   TO  WRK-CMB-CD
           PERFORM 4200-KGETCLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KGETC-G
      *
           MOVE    SPA-GMN-KGKANJINASHI
                                   TO  WRK-CMB-CD
           PERFORM 4200-KGKANJINASHILST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KGKANJINASHI-G
      *
           MOVE    SPA-GMN-KGKETACHK
                                   TO  WRK-CMB-CD
           PERFORM 4200-KGKETACHKLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KGKETACHK-G
      *
           MOVE    SPA-GMN-KGKETAMIN
                                   TO  WRK-CMB-CD
           PERFORM 4200-KGKETAMINLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KGKETAMIN-G
      *
           MOVE    SPA-GMN-NM1SUJI TO  WRK-CMB-CD
           PERFORM 4200-NMSUJILST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NM1SUJI-G
      *
           MOVE    SPA-GMN-NM1KANJI TO WRK-CMB-CD
           PERFORM 4200-NMKANJILST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NM1KANJI-G
      *
           MOVE    SPA-GMN-NM1ABT  TO  WRK-CMB-CD
           PERFORM 4200-NMABTLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NM1ABT-G
      *
           MOVE    SPA-GMN-NM1HYPN TO  WRK-CMB-CD
           PERFORM 4200-NMHYPNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NM1HYPN-G
      *
           MOVE    SPA-GMN-NM1KAKKO TO WRK-CMB-CD
           PERFORM 4200-NMKAKKOLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NM1KAKKO-G
      *
           MOVE    SPA-GMN-NM1ETC  TO  WRK-CMB-CD
           PERFORM 4200-NMETCLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NM1ETC-G
      *
           MOVE    SPA-GMN-NMSUJINASHI
                                   TO  WRK-CMB-CD
           PERFORM 4200-NMSUJINASHILST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NMSUJINASHI-G
      *
           MOVE    SPA-GMN-NMKETACHK
                                   TO  WRK-CMB-CD
           PERFORM 4200-NMKETACHKLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NMKETACHK-G
      *
           MOVE    SPA-GMN-NMKETAMIN
                                   TO  WRK-CMB-CD
           PERFORM 4200-NMKETAMINLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NMKETAMIN-G
      *
           MOVE    SPA-GMN-NM2SUJI TO  WRK-CMB-CD
           PERFORM 4200-NMSUJILST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NM2SUJI-G
      *
           MOVE    SPA-GMN-NM2KANJI TO WRK-CMB-CD
           PERFORM 4200-NMKANJILST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NM2KANJI-G
      *
           MOVE    SPA-GMN-NM2ABT  TO  WRK-CMB-CD
           PERFORM 4200-NMABTLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NM2ABT-G
      *
           MOVE    SPA-GMN-NM2HYPN TO  WRK-CMB-CD
           PERFORM 4200-NMHYPNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NM2HYPN-G
      *
           MOVE    SPA-GMN-NM2KAKKO TO WRK-CMB-CD
           PERFORM 4200-NMKAKKOLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NM2KAKKO-G
      *
           MOVE    SPA-GMN-NM2ETC  TO  WRK-CMB-CD
           PERFORM 4200-NMETCLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-NM2ETC-G
      *
           .
       4202-CHKCMB-DEFAULT-SET-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       4101-INPUT-SET-SEC          SECTION.
      *
           MOVE    W63-STYUKYMD        TO  SPA-GMN-STYUKYMD
           MOVE    W63-EDYUKYMD        TO  SPA-GMN-EDYUKYMD
           MOVE    W63-HKNJANUM        TO  SPA-GMN-HKNJANUM
           MOVE    W63-JYKNKBN         TO  SPA-GMN-JYKNKBN-G
           MOVE    W63-HKNNUM          TO  SPA-GMN-HKNNUM-G
           MOVE    W63-KGCHKPTN        TO  SPA-GMN-KGCHKPTN-G
      *
           MOVE    W63-KGSUJI          TO  SPA-GMN-KGSUJI-G
           MOVE    W63-KGKANJI         TO  SPA-GMN-KGKANJI-G
           MOVE    W63-KGABT           TO  SPA-GMN-KGABT-G
           MOVE    W63-KGHYPN          TO  SPA-GMN-KGHYPN-G
           MOVE    W63-KGKAKKO         TO  SPA-GMN-KGKAKKO-G
           MOVE    W63-KGETC           TO  SPA-GMN-KGETC-G
           MOVE    W63-KGKANJINASHI    TO  SPA-GMN-KGKANJINASHI-G
           MOVE    W63-KGKETACHK       TO  SPA-GMN-KGKETACHK-G
           MOVE    W63-KGKETAMIN       TO  SPA-GMN-KGKETAMIN-G
      *
           MOVE    W63-NM1SUJI         TO  SPA-GMN-NM1SUJI-G
           MOVE    W63-NM1KANJI        TO  SPA-GMN-NM1KANJI-G
           MOVE    W63-NM1ABT          TO  SPA-GMN-NM1ABT-G
           MOVE    W63-NM1HYPN         TO  SPA-GMN-NM1HYPN-G
           MOVE    W63-NM1KAKKO        TO  SPA-GMN-NM1KAKKO-G
           MOVE    W63-NM1ETC          TO  SPA-GMN-NM1ETC-G
           MOVE    W63-NMSUJINASHI     TO  SPA-GMN-NMSUJINASHI-G
           MOVE    W63-NMKETACHK       TO  SPA-GMN-NMKETACHK-G
           MOVE    W63-NMKETAMIN       TO  SPA-GMN-NMKETAMIN-G
      *
           MOVE    W63-NM2SUJI         TO  SPA-GMN-NM2SUJI-G
           MOVE    W63-NM2KANJI        TO  SPA-GMN-NM2KANJI-G
           MOVE    W63-NM2ABT          TO  SPA-GMN-NM2ABT-G
           MOVE    W63-NM2HYPN         TO  SPA-GMN-NM2HYPN-G
           MOVE    W63-NM2KAKKO        TO  SPA-GMN-NM2KAKKO-G
           MOVE    W63-NM2ETC          TO  SPA-GMN-NM2ETC-G
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       4101-INPUT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    選択番号チェック
           PERFORM 4101-SELNUM-CHK-SEC
      *
      *    有効期間開始日チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-STYUKYMD-CHK-SEC
           END-IF
      *
      *    有効期間終了日チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-EDYUKYMD-CHK-SEC
           END-IF
      *
      *    一致条件チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-JYKNKBN-CHK-SEC
           END-IF
      *
      *    保険者番号チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-HKNJANUM-CHK-SEC
           END-IF
      *
      *    保険の種類チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-HKNNUM-CHK-SEC
           END-IF
      *
      *    記号抜けチェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KGCHKPTN-CHK-SEC
           END-IF
      *
      *    記号数字チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KGSUJI-CHK-SEC
           END-IF
      *
      *    記号漢字チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KGKANJI-CHK-SEC
           END-IF
      *
      *    記号英字チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KGABT-CHK-SEC
           END-IF
      *
      *    記号括弧ハイフンチェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KGHYPN-CHK-SEC
           END-IF
      *
      *    記号括弧チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KGKAKKO-CHK-SEC
           END-IF
      *
      *    記号その他チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KGETC-CHK-SEC
           END-IF
      *
      *    記号漢字無しチェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KGKANJINASHI-CHK-SEC
           END-IF
      *
      *    記号桁数チェックチェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KGKETACHK-CHK-SEC
           END-IF
      *
      *    記号桁数下限チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-KGKETAMIN-CHK-SEC
           END-IF
      *
      *    番号１文字目数字チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NM1SUJI-CHK-SEC
           END-IF
      *
      *    番号１文字目漢字チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NM1KANJI-CHK-SEC
           END-IF
      *
      *    番号１文字目英字チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NM1ABT-CHK-SEC
           END-IF
      *
      *    番号１文字目括弧ハイフンチェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NM1HYPN-CHK-SEC
           END-IF
      *
      *    番号１文字目括弧チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NM1KAKKO-CHK-SEC
           END-IF
      *
      *    番号１文字目その他チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NM1ETC-CHK-SEC
           END-IF
      *
      *    番号数字無しチェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NMSUJINASHI-CHK-SEC
           END-IF
      *
      *    番号桁数チェックチェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NMKETACHK-CHK-SEC
           END-IF
      *
      *    番号桁数下限チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NMKETAMIN-CHK-SEC
           END-IF
      *
      *    番号２文字目以降数字チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NM2SUJI-CHK-SEC
           END-IF
      *
      *    番号２文字目以降漢字チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NM2KANJI-CHK-SEC
           END-IF
      *
      *    番号２文字目以降英字チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NM2ABT-CHK-SEC
           END-IF
      *
      *    番号２文字目以降括弧ハイフンチェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NM2HYPN-CHK-SEC
           END-IF
      *
      *    番号２文字目以降括弧チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NM2KAKKO-CHK-SEC
           END-IF
      *
      *    番号２文字目以降その他チェック
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-NM2ETC-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-SELNUM      =   ZERO )
               CONTINUE
           ELSE
               IF    ( SPA-GMN-SELNUM  >   SPA-GMN-CLIST-MAX )
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    001         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *
           .
      *
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効開始日チェック処理
      *****************************************************************
       4101-STYUKYMD-CHK-SEC      SECTION.
      *
      *    空白ならばALL"0"をセットする
           IF    ( SPA-GMN-STYUKYMD       =   SPACE )
            OR   ( SPA-GMN-STYUKYMD (1:3) =  "000" )
               MOVE    "00000000"      TO  SPA-GMN-STYUKYMD
                                           SPA-NAI-STYUKYMD
           END-IF
      *
           MOVE    SPA-NAI-STYUKYMD    TO  WRK-STYUKYMD
      *
           MOVE    SPACE               TO  SPA-NAI-STYUKYMD
      *
           MOVE    SPA-GMN-STYUKYMD    TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( FLG-HIZUKE          =   ZERO )
               MOVE    WRK-HENYMDG     TO  SPA-GMN-STYUKYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-STYUKYMD
      *
               IF    ( WRK-MCP-WIDGET  =   "STYUKYMD" )
      *            保険の種類コンボの内容を再セット
                   PERFORM 31011-HKNNUMLST-SET-SEC
               END-IF
           ELSE
               MOVE    WRK-STYUKYMD    TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-STYUKYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-STYUKYMD
               MOVE    "0006"          TO  SPA-ERRCD
               MOVE    006             TO  SPA-GMN-CUR
           END-IF
      *
      *
           .
      *
       4101-STYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効終了日チェック処理
      *****************************************************************
       4101-EDYUKYMD-CHK-SEC      SECTION.
      *
           IF    ( SPA-GMN-EDYUKYMD   =   SPACE )
             OR  ( SPA-GMN-STYUKYMD (1:3) =  "999" )
               MOVE    "99999999"      TO  SPA-GMN-EDYUKYMD
                                           SPA-NAI-EDYUKYMD
           END-IF
      *
           MOVE    SPA-NAI-EDYUKYMD   TO  WRK-EDYUKYMD
      *
           MOVE    SPA-GMN-EDYUKYMD   TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( FLG-HIZUKE          =   ZERO )
               MOVE    WRK-HENYMDG     TO  SPA-GMN-EDYUKYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-EDYUKYMD
           ELSE
               MOVE    WRK-EDYUKYMD    TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-EDYUKYMD
               MOVE    WRK-SYMD        TO  SPA-NAI-EDYUKYMD
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    007             TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       4101-EDYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一致条件区分チェック処理
      *****************************************************************
       4101-JYKNKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-JYKNKBN     TO  WRK-CMB-CD
           PERFORM 4200-JYKNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0"             TO  WRK-CMB-CD
               PERFORM 4200-JYKNKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-JYKNKBN-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-JYKNKBN-G
           END-IF
      *
           IF    ( SPA-GMN-JYKNKBN     =   "2" )
               MOVE    SPACE           TO  SPA-GMN-HKNJANUM
           END-IF
      *
           .
      *
       4101-JYKNKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号チェック処理
      *****************************************************************
       4101-HKNJANUM-CHK-SEC           SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-HKNJANAME
           MOVE    ZERO                TO  SPA-NAI-HKNJANUMLEN
      *
           MOVE    SPA-GMN-HKNJANUM    TO  WRK-KANACHK-DATA
           PERFORM 800-HANKAKU-CHK-SEC
           MOVE    WRK-KANACHK-DATA    TO  SPA-GMN-HKNJANUM
           IF    ( FLG-KANACHK         =   ZERO )
               MOVE    WRK-KANACHK-MAXLEN
                                       TO  SPA-NAI-HKNJANUMLEN
               IF    ( SPA-GMN-JYKNKBN     =   "0" )
                   MOVE    SPA-GMN-HKNJANUM
                                   TO  WRK-HKNJANUM
                   PERFORM 900-HKNJAINF-KEY-SEL-SEC
                   MOVE    HKNJA-HKNJANAME
                                   TO  SPA-GMN-HKNJANAME
                   IF    ( HKNJA-HKNNUM    =   "060"
                       AND SPA-GMN-HKNNUM  =   "068" )
                    OR   ( HKNJA-HKNNUM    =   "067"
                       AND SPA-GMN-HKNNUM  =   "069" )
                    OR   ( HKNJA-HKNNUM    =   "039"
                       AND SPA-GMN-HKNNUM  =   "040" )
                       MOVE    SPA-GMN-HKNNUM
                                   TO  WRK-CMB-CD
                   ELSE
                       MOVE    HKNJA-HKNNUM
                                   TO  WRK-CMB-CD
                   END-IF
                   PERFORM 4200-HKNNUMLST-CHK-SEC
                   IF    ( WRK-CMB-CD  NOT =   SPACE )
                       MOVE    WRK-CMB-ITM
                                   TO  SPA-GMN-HKNNUM-G
                       IF    ( SPA-NAI-HKNNUM-BF  =  SPACE )
                        OR   ( SPA-GMN-HKNNUM NOT =  SPA-NAI-HKNNUM-BF )
                           EVALUATE    SPA-GMN-HKNNUM
                           WHEN    CONST-H039
                               MOVE    KGNMCHK-H039-KGCHECK
                                           TO  SPA-GMN-KGCHKPTN
                           WHEN    CONST-H009
                               MOVE    KGNMCHK-H009-KGCHECK
                                           TO  SPA-GMN-KGCHKPTN
                           WHEN    OTHER
                               MOVE    KGNMCHK-HDEF-KGCHECK
                                           TO  SPA-GMN-KGCHKPTN
                           END-EVALUATE
                           PERFORM 4201-CHKCMB-DEFAULT-SET-SEC
                           PERFORM 4202-CHKCMB-DEFAULT-SET-SEC
                       END-IF
                   END-IF
               END-IF
           ELSE
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    002         TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       4101-HKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類チェック処理
      *****************************************************************
       4101-HKNNUM-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-HKNNUM-G   =    SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-HKNNUM     TO  WRK-CMB-CD
               PERFORM 4200-HKNNUMLST-CHK-SEC
               IF    ( WRK-CMB-ITM    =   SPACE )
                   MOVE    "0004"         TO  SPA-ERRCD
                   MOVE    4              TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-CMB-ITM    TO  SPA-GMN-HKNNUM-G
                   IF    ( WRK-MCP-WIDGET =   "HKNNUM" )
                       IF    ( SPA-NAI-HKNNUM-BF  =  SPACE )
                        OR   ( SPA-GMN-HKNNUM NOT =  SPA-NAI-HKNNUM-BF )
                           EVALUATE    SPA-GMN-HKNNUM
                           WHEN    CONST-H039
                               MOVE    KGNMCHK-H039-KGCHECK
                                           TO  SPA-GMN-KGCHKPTN
                           WHEN    CONST-H009
                               MOVE    KGNMCHK-H009-KGCHECK
                                           TO  SPA-GMN-KGCHKPTN
                           WHEN    OTHER
                               MOVE    KGNMCHK-HDEF-KGCHECK
                                           TO  SPA-GMN-KGCHKPTN
                           END-EVALUATE
                           PERFORM 4201-CHKCMB-DEFAULT-SET-SEC
                           PERFORM 4202-CHKCMB-DEFAULT-SET-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-HKNNUM         TO  SPA-NAI-HKNNUM-BF
      *
           .
      *
       4101-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    記号抜けチェック処理
      *****************************************************************
       4101-KGCHKPTN-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-KGCHKPTN    TO  WRK-CMB-CD
           PERFORM 4200-KGCHKPTNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0"             TO  WRK-CMB-CD
               PERFORM 4200-KGCHKPTNLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-KGCHKPTN-G
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-KGCHKPTN-G
           END-IF
      *
      *    IF    ( SPA-GMN-KGCHKPTN    =   "2" )
      *        MOVE    "2"             TO  SPA-GMN-KGSUJI
      *        MOVE    "2"             TO  SPA-GMN-KGKANJI
      *        MOVE    "2"             TO  SPA-GMN-KGABT
      *        MOVE    "2"             TO  SPA-GMN-KGHYPN
      *        MOVE    "2"             TO  SPA-GMN-KGKAKKO
      *        MOVE    "2"             TO  SPA-GMN-KGETC
      *        MOVE    "1"             TO  SPA-GMN-KGKANJINASHI
      *    END-IF
      *
           .
      *
       4101-KGCHKPTN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号数字チェック処理
      *****************************************************************
       4101-KGSUJI-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-KGSUJI    TO  WRK-CMB-CD
           PERFORM 4200-KGSUJILST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-KGSUJI TO  WRK-CMB-CD
               PERFORM 4200-KGSUJILST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGSUJI-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGSUJI-G
           END-IF
      *
           .
      *
       4101-KGSUJI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号漢字チェック処理
      *****************************************************************
       4101-KGKANJI-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-KGKANJI    TO  WRK-CMB-CD
           PERFORM 4200-KGKANJILST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-KGKANJI    TO  WRK-CMB-CD
               PERFORM 4200-KGKANJILST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGKANJI-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGKANJI-G
           END-IF
      *
           IF    ( SPA-GMN-KGKANJI     =   "2" )
               MOVE    "1"                 TO  SPA-GMN-KGKANJINASHI
           END-IF
      *
           .
      *
       4101-KGKANJI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号英字チェック処理
      *****************************************************************
       4101-KGABT-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-KGABT    TO  WRK-CMB-CD
           PERFORM 4200-KGABTLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-KGABT  TO  WRK-CMB-CD
               PERFORM 4200-KGABTLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGABT-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGABT-G
           END-IF
      *
           .
      *
       4101-KGABT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号ハイフンチェック処理
      *****************************************************************
       4101-KGHYPN-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-KGHYPN    TO  WRK-CMB-CD
           PERFORM 4200-KGHYPNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-KGHYPN TO  WRK-CMB-CD
               PERFORM 4200-KGHYPNLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGHYPN-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGHYPN-G
           END-IF
      *
           .
      *
       4101-KGHYPN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号括弧チェック処理
      *****************************************************************
       4101-KGKAKKO-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-KGKAKKO    TO  WRK-CMB-CD
           PERFORM 4200-KGKAKKOLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-KGKAKKO    TO  WRK-CMB-CD
               PERFORM 4200-KGKAKKOLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGKAKKO-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGKAKKO-G
           END-IF
      *
           .
      *
       4101-KGKAKKO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号その他チェック処理
      *****************************************************************
       4101-KGETC-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-KGETC    TO  WRK-CMB-CD
           PERFORM 4200-KGETCLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-KGETC  TO  WRK-CMB-CD
               PERFORM 4200-KGETCLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGETC-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGETC-G
           END-IF
      *
           .
      *
       4101-KGETC-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号漢字無しチェック処理
      *****************************************************************
       4101-KGKANJINASHI-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-KGKANJINASHI    TO  WRK-CMB-CD
           PERFORM 4200-KGKANJINASHILST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-KGKANJINASHI
                                           TO  WRK-CMB-CD
               PERFORM 4200-KGKANJINASHILST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGKANJINASHI-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGKANJINASHI-G
           END-IF
      *
           .
      *
       4101-KGKANJINASHI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号桁数チェックチェック処理
      *****************************************************************
       4101-KGKETACHK-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-KGKETACHK   TO  WRK-CMB-CD
           PERFORM 4200-KGKETACHKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGKETACHK-G
      *
           IF    ( SPA-GMN-KGKETACHK   =   "0" )
               MOVE    SPACE           TO  SPA-GMN-KGKETAMIN
           END-IF
      *
           .
      *
       4101-KGKETACHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号桁数下限チェック処理
      *****************************************************************
       4101-KGKETAMIN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-KGKETAMIN   TO  WRK-CMB-CD
           PERFORM 4200-KGKETAMINLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-KGKETAMIN-G
      *
           .
      *
       4101-KGKETAMIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号数字チェック処理
      *****************************************************************
       4101-NM1SUJI-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-NM1SUJI    TO  WRK-CMB-CD
           PERFORM 4200-NMSUJILST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-NM1SUJI
                                           TO  WRK-CMB-CD
               PERFORM 4200-NMSUJILST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM1SUJI-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM1SUJI-G
           END-IF
      *
           .
      *
       4101-NM1SUJI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号漢字チェック処理
      *****************************************************************
       4101-NM1KANJI-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-NM1KANJI    TO  WRK-CMB-CD
           PERFORM 4200-NMKANJILST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-NM1KANJI
                                           TO  WRK-CMB-CD
               PERFORM 4200-NMKANJILST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM1KANJI-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM1KANJI-G
           END-IF
      *
           .
      *
       4101-NM1KANJI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号英字チェック処理
      *****************************************************************
       4101-NM1ABT-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-NM1ABT    TO  WRK-CMB-CD
           PERFORM 4200-NMABTLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-NM1ABT
                                           TO  WRK-CMB-CD
               PERFORM 4200-NMABTLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM1ABT-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM1ABT-G
           END-IF
      *
           .
      *
       4101-NM1ABT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号ハイフンチェック処理
      *****************************************************************
       4101-NM1HYPN-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-NM1HYPN    TO  WRK-CMB-CD
           PERFORM 4200-NMHYPNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-NM1HYPN
                                           TO  WRK-CMB-CD
               PERFORM 4200-NMHYPNLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM1HYPN-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM1HYPN-G
           END-IF
      *
           .
      *
       4101-NM1HYPN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号括弧チェック処理
      *****************************************************************
       4101-NM1KAKKO-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-NM1KAKKO    TO  WRK-CMB-CD
           PERFORM 4200-NMKAKKOLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-NM1KAKKO
                                           TO  WRK-CMB-CD
               PERFORM 4200-NMKAKKOLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM1KAKKO-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM1KAKKO-G
           END-IF
      *
           .
      *
       4101-NM1KAKKO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号その他チェック処理
      *****************************************************************
       4101-NM1ETC-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-NM1ETC    TO  WRK-CMB-CD
           PERFORM 4200-NMETCLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-NM1ETC
                                           TO  WRK-CMB-CD
               PERFORM 4200-NMETCLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM1ETC-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM1ETC-G
           END-IF
      *
           .
      *
       4101-NM1ETC-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号数字無しチェック処理
      *****************************************************************
       4101-NMSUJINASHI-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-NMSUJINASHI     TO  WRK-CMB-CD
           PERFORM 4200-NMSUJINASHILST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-NMSUJINASHI
                                           TO  WRK-CMB-CD
               PERFORM 4200-NMSUJINASHILST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NMSUJINASHI-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NMSUJINASHI-G
           END-IF
      *
           .
      *
       4101-NMSUJINASHI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号桁数チェックチェック処理
      *****************************************************************
       4101-NMKETACHK-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-NMKETACHK   TO  WRK-CMB-CD
           PERFORM 4200-NMKETACHKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NMKETACHK-G
      *
           IF    ( SPA-GMN-NMKETACHK   =   "0" )
               MOVE    SPACE           TO  SPA-GMN-NMKETAMIN
           END-IF
      *
           .
      *
       4101-NMKETACHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号桁数下限チェック処理
      *****************************************************************
       4101-NMKETAMIN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-NMKETAMIN   TO  WRK-CMB-CD
           PERFORM 4200-NMKETAMINLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NMKETAMIN-G
      *
           .
      *
       4101-NMKETAMIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号数字チェック処理
      *****************************************************************
       4101-NM2SUJI-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-NM2SUJI    TO  WRK-CMB-CD
           PERFORM 4200-NMSUJILST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-NM2SUJI
                                           TO  WRK-CMB-CD
               PERFORM 4200-NMSUJILST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM2SUJI-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM2SUJI-G
           END-IF
      *
           .
      *
       4101-NM2SUJI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号漢字チェック処理
      *****************************************************************
       4101-NM2KANJI-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-NM2KANJI    TO  WRK-CMB-CD
           PERFORM 4200-NMKANJILST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-NM2KANJI
                                           TO  WRK-CMB-CD
               PERFORM 4200-NMKANJILST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM2KANJI-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM2KANJI-G
           END-IF
      *
           .
      *
       4101-NM2KANJI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号英字チェック処理
      *****************************************************************
       4101-NM2ABT-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-NM2ABT    TO  WRK-CMB-CD
           PERFORM 4200-NMABTLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-NM2ABT
                                           TO  WRK-CMB-CD
               PERFORM 4200-NMABTLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM2ABT-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM2ABT-G
           END-IF
      *
           .
      *
       4101-NM2ABT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号ハイフンチェック処理
      *****************************************************************
       4101-NM2HYPN-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-NM2HYPN    TO  WRK-CMB-CD
           PERFORM 4200-NMHYPNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-NM2HYPN
                                           TO  WRK-CMB-CD
               PERFORM 4200-NMHYPNLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM2HYPN-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM2HYPN-G
           END-IF
      *
           .
      *
       4101-NM2HYPN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号括弧チェック処理
      *****************************************************************
       4101-NM2KAKKO-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-NM2KAKKO    TO  WRK-CMB-CD
           PERFORM 4200-NMKAKKOLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-NM2KAKKO
                                           TO  WRK-CMB-CD
               PERFORM 4200-NMKAKKOLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM2KAKKO-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM2KAKKO-G
           END-IF
      *
           .
      *
       4101-NM2KAKKO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号その他チェック処理
      *****************************************************************
       4101-NM2ETC-CHK-SEC               SECTION.
      *
           MOVE    SPA-GMN-NM2ETC    TO  WRK-CMB-CD
           PERFORM 4200-NMETCLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    KGNMCHK-HDEF-NM2ETC
                                           TO  WRK-CMB-CD
               PERFORM 4200-NMETCLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM2ETC-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NM2ETC-G
           END-IF
      *
           .
      *
       4101-NM2ETC-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"           TO  SPA-SAKIPG
           MOVE    "W63"           TO  SPA-MOTOPG
      *
           MOVE    "CHANGE"        TO  MCP-PUTTYPE
           MOVE    "W01"           TO  MCP-WINDOW
      *
      *    ＰＵＴ　処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC                   SECTION.
      *
           INITIALIZE                  SPA-GMN-SELNUM
                                       SPA-GMN-INPUT-AREA
                                       SPA-NAI-INPUT-AREA
                                       SPA-MOT-AREA
      *
      *    有効開始日
           MOVE    SPA-SYSYMD      TO  WRK-YMD
           MOVE    "01"            TO  WRK-YMD (7:2)
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-STYUKYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-STYUKYMD
      *
      *    有効終了日
           MOVE    "99999999"      TO  SPA-GMN-EDYUKYMD
                                       SPA-NAI-EDYUKYMD
      *
      *    コンボ項目設定処理
           PERFORM 3101-COMBO-SET-SEC
      *
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-JYKNKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-JYKNKBN-G
      *
           MOVE    "0"             TO  WRK-CMB-CD
           PERFORM 4200-KGCHKPTNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-KGCHKPTN-G
      *
      *
           PERFORM 4201-CHKCMB-DEFAULT-SET-SEC
           PERFORM 4202-CHKCMB-DEFAULT-SET-SEC
      *
      *    カーソル位置セット
           IF    ( SPA-GMN-CLIST-MAX   >   ZERO )
               MOVE    001         TO  SPA-GMN-CUR
           ELSE
               MOVE    002         TO  SPA-GMN-CUR
           END-IF
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       480-TUIKA-SEC               SECTION.
      *
      *    入力時チェック
           PERFORM 4101-INPUT-SET-SEC
      *
           MOVE    CONST-INSERT    TO  WRK-UPDKBN
      *
      *    関連チェック
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
      *    履歴チェック
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4901-RIREKI-CHK-SEC
           END-IF
      *
      *
      *    チェック排除情報セット処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4701-KOUMOKU-SET-SEC
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
      *
      *    既存データの区分コードを更新
           IF    ( SPA-ERRCD       =   SPACE )
      *
               INITIALIZE                  CHK004-REC
               MOVE    "1102"          TO  CHK004-KANRICD
               PERFORM 900-CHK004-KEY9-SEL-SEC
               PERFORM UNTIL ( FLG-CHK004  NOT =   ZERO )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   MOVE    CHK004-KBNCD    TO  WRK-KBNCD-G
                   COMPUTE WRK-KBNCD   =   WRK-KBNCD   +   1
      *
                   MOVE    CHK004-KEY      TO  CHK004-UPDKEY
                   MOVE    WRK-KBNCD-G     TO  CHK004-KBNCD
                   MOVE    SMCNDATE-YMD    TO  CHK004-UPYMD
                   MOVE    SMCNDATE-HMS    TO  CHK004-UPHMS
                   MOVE    "tbl_chk004"    TO  MCP-TABLE
                   MOVE    "upd1"          TO  MCP-PATHNAME
                   MOVE    CHK004-REC      TO  MCPDATA-REC
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC          =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE   "4007"           TO  SPA-ERRCD
                       MOVE    002             TO  SPA-GMN-CUR
                   END-IF
      *
                   PERFORM 900-CHK004-KEY9-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_chk004"    TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
      *    チェック排除情報マスタ追加
           IF    ( SPA-ERRCD        =   SPACE )
               MOVE    CK4-1102-REC    TO  MCPDATA-REC
               MOVE    "tbl_chk004"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE   "4001"           TO  SPA-ERRCD
                   MOVE    002             TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD        =   SPACE )
               PERFORM 3101-CLIST-SET-SEC
               MOVE    2           TO  FLG-CLIST-ROW
      *
               PERFORM 420-CLEAR-SEC
      *
               IF    ( SPA-GMN-CLIST-MAX   >=  CONST-CLIST-MAX )
                   MOVE    "1005"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       480-TUIKA-EXT.
           EXIT.
      *****************************************************************
      *    チェック排除情報セット処理
      *****************************************************************
       4701-KOUMOKU-SET-SEC        SECTION.
      *
           IF    ( WRK-UPDKBN      =   CONST-UPDATE )
               INITIALIZE                  CK4-1102-REC
               MOVE    "1102"          TO  CK4-1102-KANRICD
               MOVE    SPA-MOT-KBNCD   TO  CK4-1102-KBNCD
               MOVE    SPA-MOT-STYUKYMD
                                       TO  CK4-1102-STYUKYMD
                                           CK4-1102-EDYUKYMD
               MOVE    CK4-1102-REC    TO  CHK004-REC
               PERFORM 900-CHK004-KEY-SEL-SEC
               MOVE    CHK004-REC       TO  CK4-1102-REC
           ELSE
               INITIALIZE                  CK4-1102-REC
               MOVE    SPA-HOSPNUM     TO  CK4-1102-HOSPNUM
               MOVE    "1102"          TO  CK4-1102-KANRICD
               INITIALIZE                  WRK-KBNCD-G
               MOVE    1               TO  WRK-KBNCD
               MOVE    WRK-KBNCD-G     TO  CK4-1102-KBNCD
           END-IF
      *
           MOVE    SPA-NAI-STYUKYMD    TO  CK4-1102-STYUKYMD
           MOVE    SPA-NAI-EDYUKYMD    TO  CK4-1102-EDYUKYMD
           MOVE    SPA-GMN-HKNJANUM    TO  CK4-1102-HKNJANUM
           MOVE    SPA-NAI-HKNJANUMLEN TO  CK4-1102-HKNJANUMLEN
           MOVE    SPA-GMN-JYKNKBN     TO  CK4-1102-JYKNKBN
           MOVE    SPA-GMN-HKNNUM      TO  CK4-1102-HKNNUM
           IF    ( SPA-GMN-KGCHKPTN    =   "0" OR "2" )
               MOVE    "0"             TO  CK4-1102-KIGOCHK
           ELSE
               MOVE    "1"             TO  CK4-1102-KIGOCHK
           END-IF
           MOVE    SPA-GMN-KGCHKPTN    TO  CK4-1102-KGCHKPTN
           MOVE    SPA-GMN-KGSUJI      TO  CK4-1102-KGSUJI
           MOVE    SPA-GMN-KGKANJI     TO  CK4-1102-KGKANJI
           MOVE    SPA-GMN-KGABT       TO  CK4-1102-KGABT
           MOVE    SPA-GMN-KGHYPN      TO  CK4-1102-KGHYPN
           MOVE    SPA-GMN-KGKAKKO     TO  CK4-1102-KGKAKKO
           MOVE    SPA-GMN-KGETC       TO  CK4-1102-KGETC
           MOVE    SPA-GMN-KGKANJINASHI TO CK4-1102-KGKANJINASHI
           MOVE    SPA-GMN-KGKETAMIN   TO  CK4-1102-KGKETAMIN
                                           CK4-1102-KGKETAMAX
           MOVE    SPA-GMN-NMSUJINASHI TO  CK4-1102-NMSUJINASHI
           MOVE    SPA-GMN-NMKETAMIN   TO  CK4-1102-NMKETAMIN
                                           CK4-1102-NMKETAMAX
           MOVE    "0"                 TO  CK4-1102-NM1CHKPTN
           MOVE    SPA-GMN-NM1SUJI     TO  CK4-1102-NM1SUJI
           MOVE    SPA-GMN-NM1KANJI    TO  CK4-1102-NM1KANJI
           MOVE    SPA-GMN-NM1ABT      TO  CK4-1102-NM1ABT
           MOVE    SPA-GMN-NM1HYPN     TO  CK4-1102-NM1HYPN
           MOVE    SPA-GMN-NM1KAKKO    TO  CK4-1102-NM1KAKKO
           MOVE    SPA-GMN-NM1ETC      TO  CK4-1102-NM1ETC
           MOVE    "0"                 TO  CK4-1102-NM2CHKPTN
           MOVE    SPA-GMN-NM2SUJI     TO  CK4-1102-NM2SUJI
           MOVE    SPA-GMN-NM2KANJI    TO  CK4-1102-NM2KANJI
           MOVE    SPA-GMN-NM2ABT      TO  CK4-1102-NM2ABT
           MOVE    SPA-GMN-NM2HYPN     TO  CK4-1102-NM2HYPN
           MOVE    SPA-GMN-NM2KAKKO    TO  CK4-1102-NM2KAKKO
           MOVE    SPA-GMN-NM2ETC      TO  CK4-1102-NM2ETC
           MOVE    "1"                 TO  CK4-1102-UPDDUMMY
      *
           .
       4701-KOUMOKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    変更処理
      *****************************************************************
       480-HENKOU-SEC              SECTION.
      *
      *    入力時チェック
           PERFORM 4101-INPUT-SET-SEC
      *
           MOVE    CONST-UPDATE    TO  WRK-UPDKBN
      *
      *    関連チェック
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
      *    履歴チェック
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4901-RIREKI-CHK-SEC
           END-IF
      *
      *    チェック排除情報セット処理
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4701-KOUMOKU-SET-SEC
           END-IF
      *
      *    更新用キーをセット
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  CK4-1102-UPYMD
               MOVE    SMCNDATE-HMS        TO  CK4-1102-UPHMS
               MOVE    CK4-1102-REC        TO  CHK004-REC
               MOVE    "1102"              TO  CHK004-UPKANRICD
               MOVE    SPA-MOT-KBNCD       TO  CHK004-UPKBNCD
               MOVE    SPA-MOT-STYUKYMD    TO  CHK004-UPSTYUKYMD
               MOVE    SPA-MOT-EDYUKYMD    TO  CHK004-UPEDYUKYMD
               MOVE    SPA-HOSPNUM         TO  CHK004-UPHOSPNUM
      *
               MOVE    SPA-HOSPNUM         TO  CHK004-HOSPNUM
               MOVE    CHK004-REC          TO  MCPDATA-REC
               MOVE    "tbl_chk004"        TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC          =   ZERO )
      *
      *            チェック排除情報リスト編集処理
                   PERFORM 3101-CLIST-SET-SEC
      *
      *            入力項目を初期化する
                   PERFORM 420-CLEAR-SEC
               ELSE
      *            更新失敗エラー
                   MOVE   "4002"           TO  SPA-ERRCD
                   MOVE    002             TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       480-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    削除確認処理
      *****************************************************************
       480-SAKUJO-SEC              SECTION.
      *
           MOVE    CONST-DELETE    TO  WRK-UPDKBN
      *
      *    入力項目の関連チェック処理
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  480-SAKUJO-EXT
           END-IF
      *
      *    削除するか確認する
           MOVE    "3001"          TO  SPA-WIDCD
      *
           .
       480-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    リスト項目ＵＰ処理
      *****************************************************************
       480-CHECKLIST-ITEM-UP-SEC   SECTION.
      *
           IF    ( SPA-GMN-SELNUM  <=  1 )
               CONTINUE
           ELSE
               COMPUTE WRK-LST =   SPA-GMN-SELNUM - 1
               PERFORM 4801-CHECKLIST-ITEM-UPD-SEC
           END-IF
      *
           .
       480-CHECKLIST-ITEM-UP-EXT.
           EXIT.
      *****************************************************************
      *    リスト項目ＤＯＷＮ処理
      *****************************************************************
       480-CHECKLIST-ITEM-DOWN-SEC   SECTION.
      *
           IF    ( SPA-GMN-SELNUM  =  ZERO )
            OR   ( SPA-GMN-SELNUM >=  SPA-GMN-CLIST-MAX )
               CONTINUE
           ELSE
               COMPUTE WRK-LST =   SPA-GMN-SELNUM + 1
               PERFORM 4801-CHECKLIST-ITEM-UPD-SEC
           END-IF
      *
           .
       480-CHECKLIST-ITEM-DOWN-EXT.
           EXIT.
      *****************************************************************
      *    リスト項目更新処理
      *****************************************************************
       4801-CHECKLIST-ITEM-UPD-SEC     SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           INITIALIZE                      CHK004-REC
           MOVE    "000000"            TO  CHK004-KBNCD
           MOVE    SMCNDATE-YMD        TO  CHK004-UPYMD
           MOVE    SMCNDATE-HMS        TO  CHK004-UPHMS
      *
           MOVE    SPA-HOSPNUM         TO  CHK004-UPHOSPNUM
           MOVE    "1102"              TO  CHK004-UPKANRICD
           MOVE    SPA-MOT-KBNCD       TO  CHK004-UPKBNCD
           MOVE    SPA-MOT-STYUKYMD    TO  CHK004-UPSTYUKYMD
           MOVE    SPA-MOT-EDYUKYMD    TO  CHK004-UPEDYUKYMD
           MOVE    CHK004-REC          TO  WKCHK004-REC
      *
           MOVE    CHK004-REC          TO  MCPDATA-REC
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "upd2"              TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC              =   ZERO )
               CONTINUE
           ELSE
               MOVE    "4004"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               INITIALIZE                      CHK004-REC
               MOVE    WKCHK004-UPKBNCD    TO  CHK004-KBNCD
               MOVE    SMCNDATE-YMD        TO  CHK004-UPYMD
               MOVE    SMCNDATE-HMS        TO  CHK004-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  CHK004-UPHOSPNUM
               MOVE    "1102"              TO  CHK004-UPKANRICD
               MOVE    SPA-NAI-TKBNCD    (WRK-LST)
                                           TO  CHK004-UPKBNCD
               MOVE    SPA-NAI-TSTYUKYMD (WRK-LST)
                                           TO  CHK004-UPSTYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (WRK-LST)
                                           TO  CHK004-UPEDYUKYMD
               MOVE    CHK004-REC          TO  MCPDATA-REC
               MOVE    "tbl_chk004"        TO  MCP-TABLE
               MOVE    "upd2"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC              =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "4005"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               INITIALIZE                      CHK004-REC
               MOVE    SPA-NAI-TKBNCD    (WRK-LST)
                                           TO  CHK004-KBNCD
               MOVE    SMCNDATE-YMD        TO  CHK004-UPYMD
               MOVE    SMCNDATE-HMS        TO  CHK004-UPHMS
      *
               MOVE    WKCHK004-UPDKEY     TO  CHK004-UPDKEY
               MOVE    "000000"            TO  CHK004-UPKBNCD
               MOVE    CHK004-REC          TO  MCPDATA-REC
               MOVE    "tbl_chk004"        TO  MCP-TABLE
               MOVE    "upd2"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC              =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "4006"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 3101-CLIST-SET-SEC
               MOVE    WRK-LST     TO  W63-SELNUM
               PERFORM 4100-SELNUM-CHK-SEC
               MOVE    002             TO  SPA-GMN-CUR
           END-IF
      *
           .
       4801-CHECKLIST-ITEM-UPD-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       481-SAKUJO-SEC                  SECTION.
      *
           INITIALIZE                      CK4-1102-REC
           MOVE    "1102"              TO  CK4-1102-KANRICD
           MOVE    SPA-MOT-KBNCD       TO  CK4-1102-KBNCD
           MOVE    SPA-MOT-STYUKYMD    TO  CK4-1102-STYUKYMD
           MOVE    SPA-MOT-EDYUKYMD    TO  CK4-1102-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  CK4-1102-HOSPNUM
           MOVE    CK4-1102-REC        TO  MCPDATA-REC
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC              =   ZERO )
      *        チェック排除情報リスト編集処理
               PERFORM 3101-CLIST-SET-SEC
      *        入力項目を初期化する
               PERFORM 420-CLEAR-SEC
           ELSE
      *        削除失敗エラー
               MOVE    "4003"          TO  SPA-ERRCD
               MOVE    002             TO  SPA-GMN-CUR
           END-IF
      *
           .    
       481-SAKUJO-EXT.
           EXIT.
      *
      *****************************************************************
      *    一致条件コンボチェック処理
      *****************************************************************
       4200-JYKNKBNLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JYKNKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-JYKNKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-JYKNKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-JYKNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険番号コンボチェック処理
      *****************************************************************
       4200-HKNNUMLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-HKNNUM-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-HKNNUML (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-HKNNUMLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-HKNNUMLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号抜けコンボボックスチェック処理
      *****************************************************************
       4200-KGCHKPTNLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KGCHKPTN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KGCHKPTNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-KGCHKPTNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KGCHKPTNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号数字コンボボックスチェック処理
      *****************************************************************
       4200-KGSUJILST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KGSUJI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KGSUJIL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-KGSUJILST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KGSUJILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号漢字コンボボックスチェック処理
      *****************************************************************
       4200-KGKANJILST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KGKANJI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KGKANJIL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-KGKANJILST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KGKANJILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号英字コンボボックスチェック処理
      *****************************************************************
       4200-KGABTLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KGABT-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KGABTL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-KGABTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KGABTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号ハイフンコンボボックスチェック処理
      *****************************************************************
       4200-KGHYPNLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KGHYPN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KGHYPNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-KGHYPNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KGHYPNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号括弧コンボボックスチェック処理
      *****************************************************************
       4200-KGKAKKOLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KGKAKKO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KGKAKKOL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-KGKAKKOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KGKAKKOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号括弧コンボボックスチェック処理
      *****************************************************************
       4200-KGETCLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KGETC-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KGETCL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-KGETCLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KGETCLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号漢字無しコンボボックスチェック処理
      *****************************************************************
       4200-KGKANJINASHILST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KGKANJINASHI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KGKANJINASHIL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-KGKANJINASHILST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KGKANJINASHILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    桁数チェックコンボボックスチェック処理
      *****************************************************************
       4200-KGKETACHKLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KGKETACHK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KGKETACHKL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-KGKETACHKLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KGKETACHKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    桁数下限コンボボックスチェック処理
      *****************************************************************
       4200-KGKETAMINLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-KGKETAMIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-KGKETAMINL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-KGKETAMINLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KGKETAMINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号数字コンボボックスチェック処理
      *****************************************************************
       4200-NMSUJILST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NMSUJI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NMSUJIL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NMSUJILST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NMSUJILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号漢字コンボボックスチェック処理
      *****************************************************************
       4200-NMKANJILST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NMKANJI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NMKANJIL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NMKANJILST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NMKANJILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号英字コンボボックスチェック処理
      *****************************************************************
       4200-NMABTLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NMABT-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NMABTL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NMABTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NMABTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号ハイフンコンボボックスチェック処理
      *****************************************************************
       4200-NMHYPNLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NMHYPN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NMHYPNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NMHYPNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NMHYPNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号括弧コンボボックスチェック処理
      *****************************************************************
       4200-NMKAKKOLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NMKAKKO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NMKAKKOL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NMKAKKOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NMKAKKOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号括弧コンボボックスチェック処理
      *****************************************************************
       4200-NMETCLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NMETC-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NMETCL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NMETCLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NMETCLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号漢字無しコンボボックスチェック処理
      *****************************************************************
       4200-NMSUJINASHILST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NMSUJINASHI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NMSUJINASHIL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NMSUJINASHILST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NMSUJINASHILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    桁数チェックコンボボックスチェック処理
      *****************************************************************
       4200-NMKETACHKLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NMKETACHK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NMKETACHKL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NMKETACHKLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NMKETACHKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    桁数下限コンボボックスチェック処理
      *****************************************************************
       4200-NMKETAMINLST-CHK-SEC            SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-NMKETAMIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NMKETAMINL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-NMKETAMINLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NMKETAMINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC          SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF  ( SPA-NAI-EDYUKYMD     <   SPA-NAI-STYUKYMD )
                   MOVE    "1002"          TO  SPA-ERRCD
                   MOVE    007             TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-SELNUM  =   ZERO )
                   IF    ( WRK-UPDKBN      =   CONST-INSERT )
                       CONTINUE
                   ELSE
                       MOVE    "1001"          TO  SPA-ERRCD
                       MOVE    001             TO  SPA-GMN-CUR
                   END-IF
               ELSE
                   IF    ( WRK-UPDKBN      =   CONST-UPDATE )
                     OR  ( WRK-UPDKBN      =   CONST-DELETE )
                       CONTINUE
                   ELSE
                       MOVE    "1003"          TO  SPA-ERRCD
                       MOVE    001             TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-JYKNKBN =   "0" )
                   IF    ( SPA-GMN-HKNJANUM    =   SPACE )
                       MOVE    "1007"          TO  SPA-ERRCD
                       MOVE    002             TO  SPA-GMN-CUR
                   ELSE
                       IF    ( SPA-GMN-HKNJANAME   =   SPACE )
                           MOVE    "1008"      TO  SPA-ERRCD
                           MOVE    002         TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-HKNNUM-G    =   SPACE )
                   MOVE    "1009"      TO  SPA-ERRCD
                   MOVE    004         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-KGKETACHK   =   "1" )
                   IF    ( SPA-GMN-KGKETAMIN-G =   SPACE )
                       MOVE    "1010"      TO  SPA-ERRCD
                       MOVE    030         TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-GMN-NMKETACHK   =   "1" )
                   IF    ( SPA-GMN-NMKETAMIN-G =   SPACE )
                       MOVE    "1011"      TO  SPA-ERRCD
                       MOVE    028         TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    履歴チェック処理
      *****************************************************************
       4901-RIREKI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-DABURI
           MOVE    LOW-VALUE           TO  WRK-KBNCD-G
      *
           INITIALIZE                  CK4-1102-REC
           MOVE    "1102"          TO  CK4-1102-KANRICD
           MOVE    CK4-1102-REC    TO  CHK004-REC
           PERFORM 900-CHK004-KEY3-SEL-SEC
           MOVE    CHK004-REC       TO  CK4-1102-REC
      *
           PERFORM UNTIL ( FLG-CHK004   NOT =   ZERO )
      *
               IF    ( CK4-1102-KBNCD  >   WRK-KBNCD-G )
                   MOVE    CK4-1102-KBNCD  TO  WRK-KBNCD-G
               END-IF
      *
               IF        ( WRK-UPDKBN          =   CONST-UPDATE )
                   AND   ( SPA-MOT-KBNCD       =   CK4-1102-KBNCD )
                   AND   ( SPA-MOT-STYUKYMD    =   CK4-1102-STYUKYMD )
                   AND   ( SPA-MOT-EDYUKYMD    =   CK4-1102-EDYUKYMD )
                       CONTINUE
               ELSE
                   IF    ( SPA-GMN-HKNJANUM    =   CK4-1102-HKNJANUM )
                    AND  ( SPA-GMN-JYKNKBN     =   CK4-1102-JYKNKBN  )
                    AND  ( SPA-GMN-HKNNUM      =   CK4-1102-HKNNUM   )
                    AND  ((SPA-NAI-EDYUKYMD   >=   CK4-1102-STYUKYMD )
                     AND  (SPA-NAI-STYUKYMD   <=   CK4-1102-EDYUKYMD ))
                       MOVE     1              TO  FLG-DABURI
                   END-IF
               END-IF
      *
               PERFORM 900-CHK004-KEY3-FET-SEC
               MOVE    CHK004-REC       TO  CK4-1102-REC
      *
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-DABURI    =   1 )
               MOVE    "1004"          TO  SPA-ERRCD
               MOVE    006             TO  SPA-GMN-CUR
           END-IF
      *
           .
       4901-RIREKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
      *    画面編集処理
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
      *        エラーメッセージ表示処理
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
      *        ガイドメッセージ表示処理
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "W63    "           TO  MCP-WINDOW
      *
      *    ＰＵＴ　処理
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    W63-CHECKLIST-ROW   TO  WRK-CLIST-ROW
      *
           INITIALIZE                  W63
      *
           IF    ( SPA-GMN-CLIST-MAX   >=  CONST-CLIST-MAX )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W63-B06-STATE
           ELSE
               MOVE    WIDGET-NORMAL
                                   TO  W63-B06-STATE
           END-IF
      *
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >       SPA-GMN-CLIST-MAX )
      *
               MOVE    SPA-GMN-TNUM  (IDX-LST)
                                       TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  W63-TNUM (IDX-LST)
               MOVE    SPA-GMN-THKNJANUM (IDX-LST)
                                       TO  W63-THKNJANUM (IDX-LST)
               MOVE    SPA-GMN-TJYKNKBN  (IDX-LST)
                                       TO  W63-TJYKNKBN  (IDX-LST)
               MOVE    SPA-GMN-THKNJANAME(IDX-LST)
                                       TO  W63-THKNJANAME(IDX-LST)
               MOVE    SPA-GMN-THKNNUM   (IDX-LST)
                                       TO  W63-THKNNUM   (IDX-LST)
               MOVE    SPA-GMN-TKIGOCHK  (IDX-LST)
                                       TO  W63-TKIGOCHK  (IDX-LST)
               MOVE    SPA-GMN-TSTYUKYMD (IDX-LST)
                                       TO  W63-TSTYUKYMD (IDX-LST)
               MOVE    SPA-GMN-TEDYUKYMD (IDX-LST)
                                       TO  W63-TEDYUKYMD (IDX-LST)
      *
               IF    ( IDX-LST         =   SPA-GMN-SELNUM )
                   MOVE    "T"         TO  W63-CHECKLIST-SELECT(IDX-LST)
               ELSE
                   MOVE    "F"         TO  W63-CHECKLIST-SELECT(IDX-LST)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-CLIST-MAX   TO  W63-CHECKLIST-COUNT
      *
           MOVE    SPA-GMN-SELNUM      TO  W63-SELNUM
      *
           MOVE    SPA-GMN-STYUKYMD    TO  W63-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  W63-EDYUKYMD
           MOVE    SPA-GMN-HKNJANUM    TO  W63-HKNJANUM
           MOVE    SPA-GMN-HKNJANAME   TO  W63-HKNJANAME
      *
           MOVE    SPA-GMN-JYKNKBN-G   TO  W63-JYKNKBN
           MOVE    SPA-GMN-JYKNKBN-MAX TO  W63-JYKNKBN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-JYKNKBN-MAX )
      *
               MOVE    SPA-GMN-JYKNKBNLST (IDX-CMB)
                                       TO  W63-JYKNKBN-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-HKNNUM-G    TO   W63-HKNNUM
           MOVE    SPA-GMN-HKNNUM-MAX  TO   W63-HKNNUM-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-HKNNUM-MAX )
      *
               MOVE    SPA-GMN-HKNNUMLST (IDX-CMB)
                                       TO  W63-HKNNUM-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-KGCHKPTN-G      TO  W63-KGCHKPTN
           MOVE    SPA-GMN-KGCHKPTN-MAX    TO  W63-KGCHKPTN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-KGCHKPTN-MAX )
      *
               MOVE    SPA-GMN-KGCHKPTNLST (IDX-CMB)
                                           TO  W63-KGCHKPTN-ITEM
                                                              (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-KGSUJI-G      TO  W63-KGSUJI
           MOVE    SPA-GMN-KGSUJI-MAX    TO  W63-KGSUJI-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-KGSUJI-MAX )
      *
               MOVE    SPA-GMN-KGSUJILST (IDX-CMB)
                                           TO  W63-KGSUJI-ITEM
                                                              (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-KGKANJI-G      TO  W63-KGKANJI
           MOVE    SPA-GMN-KGKANJI-MAX    TO  W63-KGKANJI-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-KGKANJI-MAX )
      *
               MOVE    SPA-GMN-KGKANJILST (IDX-CMB)
                                           TO  W63-KGKANJI-ITEM
                                                              (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-KGABT-G      TO  W63-KGABT
           MOVE    SPA-GMN-KGABT-MAX    TO  W63-KGABT-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-KGABT-MAX )
      *
               MOVE    SPA-GMN-KGABTLST (IDX-CMB)
                                           TO  W63-KGABT-ITEM
                                                              (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-KGHYPN-G      TO  W63-KGHYPN
           MOVE    SPA-GMN-KGHYPN-MAX    TO  W63-KGHYPN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-KGHYPN-MAX )
      *
               MOVE    SPA-GMN-KGHYPNLST (IDX-CMB)
                                           TO  W63-KGHYPN-ITEM
                                                              (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-KGKAKKO-G      TO  W63-KGKAKKO
           MOVE    SPA-GMN-KGKAKKO-MAX    TO  W63-KGKAKKO-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-KGKAKKO-MAX )
      *
               MOVE    SPA-GMN-KGKAKKOLST (IDX-CMB)
                                           TO  W63-KGKAKKO-ITEM
                                                              (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-KGETC-G      TO  W63-KGETC
           MOVE    SPA-GMN-KGETC-MAX    TO  W63-KGETC-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-KGETC-MAX )
      *
               MOVE    SPA-GMN-KGETCLST (IDX-CMB)
                                           TO  W63-KGETC-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-KGKANJINASHI-G  TO  W63-KGKANJINASHI
           MOVE    SPA-GMN-KGKANJINASHI-MAX TO W63-KGKANJINASHI-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-KGKANJINASHI-MAX )
      *
               MOVE    SPA-GMN-KGKANJINASHILST (IDX-CMB)
                                           TO  W63-KGKANJINASHI-ITEM
                                                              (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-KGKETACHK-G     TO  W63-KGKETACHK
           MOVE    SPA-GMN-KGKETAMIN-G     TO  W63-KGKETAMIN
      *
           MOVE    SPA-GMN-KGKETACHK-MAX TO  W63-KGKETACHK-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-KGKETACHK-MAX )
      *
               MOVE    SPA-GMN-KGKETACHKLST (IDX-CMB)
                                         TO  W63-KGKETACHK-ITEM(IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-KGKETAMIN-MAX TO  W63-KGKETAMIN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-KGKETAMIN-MAX )
      *
               MOVE    SPA-GMN-KGKETAMINLST (IDX-CMB)
                                         TO  W63-KGKETAMIN-ITEM(IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-NM1SUJI-G       TO  W63-NM1SUJI
           MOVE    SPA-GMN-NM1KANJI-G      TO  W63-NM1KANJI
           MOVE    SPA-GMN-NM1ABT-G        TO  W63-NM1ABT
           MOVE    SPA-GMN-NM1HYPN-G       TO  W63-NM1HYPN
           MOVE    SPA-GMN-NM1KAKKO-G      TO  W63-NM1KAKKO
           MOVE    SPA-GMN-NM1ETC-G        TO  W63-NM1ETC
           MOVE    SPA-GMN-NMSUJINASHI-G   TO  W63-NMSUJINASHI
           MOVE    SPA-GMN-NMKETACHK-G     TO  W63-NMKETACHK
           MOVE    SPA-GMN-NMKETAMIN-G     TO  W63-NMKETAMIN
      *
           MOVE    SPA-GMN-NM2SUJI-G       TO  W63-NM2SUJI
           MOVE    SPA-GMN-NM2KANJI-G      TO  W63-NM2KANJI
           MOVE    SPA-GMN-NM2ABT-G        TO  W63-NM2ABT
           MOVE    SPA-GMN-NM2HYPN-G       TO  W63-NM2HYPN
           MOVE    SPA-GMN-NM2KAKKO-G      TO  W63-NM2KAKKO
           MOVE    SPA-GMN-NM2ETC-G        TO  W63-NM2ETC
      *
           MOVE    SPA-GMN-NMSUJI-MAX      TO  W63-NM1SUJI-COUNT
                                               W63-NM2SUJI-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-NMSUJI-MAX )
      *
               MOVE    SPA-GMN-NMSUJILST (IDX-CMB)
                                   TO  W63-NM1SUJI-ITEM (IDX-CMB)
                                       W63-NM2SUJI-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-NMKANJI-MAX     TO  W63-NM1KANJI-COUNT
                                               W63-NM2KANJI-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-NMKANJI-MAX )
      *
               MOVE    SPA-GMN-NMKANJILST (IDX-CMB)
                                   TO  W63-NM1KANJI-ITEM (IDX-CMB)
                                       W63-NM2KANJI-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-NMABT-MAX       TO  W63-NM1ABT-COUNT
                                               W63-NM2ABT-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-NMABT-MAX )
      *
               MOVE    SPA-GMN-NMABTLST (IDX-CMB)
                                   TO  W63-NM1ABT-ITEM (IDX-CMB)
                                       W63-NM2ABT-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-NMHYPN-MAX      TO  W63-NM1HYPN-COUNT
                                               W63-NM2HYPN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-NMHYPN-MAX )
      *
               MOVE    SPA-GMN-NMHYPNLST (IDX-CMB)
                                   TO  W63-NM1HYPN-ITEM (IDX-CMB)
                                       W63-NM2HYPN-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
      *
           MOVE    SPA-GMN-NMKAKKO-MAX     TO  W63-NM1KAKKO-COUNT
                                               W63-NM2KAKKO-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-NMKAKKO-MAX )
      *
               MOVE    SPA-GMN-NMKAKKOLST (IDX-CMB)
                                   TO  W63-NM1KAKKO-ITEM (IDX-CMB)
                                       W63-NM2KAKKO-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-NMETC-MAX       TO  W63-NM1ETC-COUNT
                                               W63-NM2ETC-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-NMETC-MAX )
      *
               MOVE    SPA-GMN-NMETCLST (IDX-CMB)
                                   TO  W63-NM1ETC-ITEM (IDX-CMB)
                                       W63-NM2ETC-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-NMSUJINASHI-MAX TO  W63-NMSUJINASHI-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-NMSUJINASHI-MAX )
      *
               MOVE    SPA-GMN-NMSUJINASHILST (IDX-CMB)
                                   TO  W63-NMSUJINASHI-ITEM (IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-NMKETACHK-MAX TO  W63-NMKETACHK-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-NMKETACHK-MAX )
      *
               MOVE    SPA-GMN-NMKETACHKLST (IDX-CMB)
                                         TO  W63-NMKETACHK-ITEM(IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-NMKETAMIN-MAX TO  W63-NMKETAMIN-COUNT
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >       SPA-GMN-NMKETAMIN-MAX )
      *
               MOVE    SPA-GMN-NMKETAMINLST (IDX-CMB)
                                         TO  W63-NMKETAMIN-ITEM(IDX-CMB)
      *
           END-PERFORM
      *
           MOVE    WIDGET-NORMAL          TO   W63-KGKETAMIN-STATE
                                               W63-KGKETAMIN-ITEM-STATE
           IF    ( SPA-GMN-KGKETACHK      =    "0" )
               MOVE    WIDGET-INSENSITIVE TO   W63-KGKETAMIN-STATE
                                               W63-KGKETAMIN-ITEM-STATE
           END-IF
      *
           MOVE    WIDGET-NORMAL          TO   W63-NMKETAMIN-STATE
                                               W63-NMKETAMIN-ITEM-STATE
           IF    ( SPA-GMN-NMKETACHK      =    "0" )
               MOVE    WIDGET-INSENSITIVE TO   W63-NMKETAMIN-STATE
                                               W63-NMKETAMIN-ITEM-STATE
           END-IF
      *
           EVALUATE    FLG-CLIST-ROW
           WHEN    1
               MOVE    ZERO               TO   W63-CHECKLIST-ROW
               MOVE    ZERO               TO   W63-CHECKLIST-ROWATTR
           WHEN    2
               MOVE    SPA-GMN-CLIST-MAX  TO   W63-CHECKLIST-ROW
               MOVE    1                  TO   W63-CHECKLIST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-CLIST-ROW      TO   W63-CHECKLIST-ROW
               MOVE    ZERO               TO   W63-CHECKLIST-ROWATTR
           END-EVALUATE
      *
      *    カーソルセット
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)
              AND (SPA-GMN-CUR         =   ZERO )
      *        入力個所のセット処理
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
      *    カーソルセット
           EVALUATE    SPA-GMN-CUR
           WHEN    001
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    002
               MOVE    "HKNJANUM"      TO  MCP-WIDGET
           WHEN    003
               MOVE    "JYKNKBN"       TO  MCP-WIDGET
           WHEN    004
               MOVE    "HKNNUM"        TO  MCP-WIDGET
           WHEN    005
               MOVE    "KGCHKPTN"      TO  MCP-WIDGET
           WHEN    008
               MOVE    "KGSUJI"        TO  MCP-WIDGET
           WHEN    009
               MOVE    "KGKANJI"       TO  MCP-WIDGET
           WHEN    010
               MOVE    "KGABT"         TO  MCP-WIDGET
           WHEN    011
               MOVE    "KGHYPN"        TO  MCP-WIDGET
           WHEN    012
               MOVE    "KGKAKKO"       TO  MCP-WIDGET
           WHEN    013
               MOVE    "KGETC"         TO  MCP-WIDGET
           WHEN    014
               MOVE    "KGKANJINASHI"  TO  MCP-WIDGET
           WHEN    015
               MOVE    "NM1SUJI"       TO  MCP-WIDGET
           WHEN    016
               MOVE    "NM1KANJI"      TO  MCP-WIDGET
           WHEN    017
               MOVE    "NM1ABT"        TO  MCP-WIDGET
           WHEN    018
               MOVE    "NM1HYPN"       TO  MCP-WIDGET
           WHEN    019
               MOVE    "NM1KAKKO"      TO  MCP-WIDGET
           WHEN    020
               MOVE    "NM1ETC"        TO  MCP-WIDGET
           WHEN    021
               MOVE    "NMSUJINASHI"   TO  MCP-WIDGET
           WHEN    022
               MOVE    "NM2SUJI"       TO  MCP-WIDGET
           WHEN    023
               MOVE    "NM2KANJI"      TO  MCP-WIDGET
           WHEN    024
               MOVE    "NM2ABT"        TO  MCP-WIDGET
           WHEN    025
               MOVE    "NM2HYPN"       TO  MCP-WIDGET
           WHEN    026
               MOVE    "NM2KAKKO"      TO  MCP-WIDGET
           WHEN    027
               MOVE    "NM2ETC"        TO  MCP-WIDGET
           WHEN    006
               MOVE    "STYUKYMD"      TO  MCP-WIDGET
           WHEN    007
               MOVE    "EDYUKYMD"      TO  MCP-WIDGET
           WHEN    028
               MOVE    "NMKETAMIN"     TO  MCP-WIDGET
           WHEN    029
               MOVE    "NMKETACHK"     TO  MCP-WIDGET
           WHEN    030
               MOVE    "KGKETAMIN"     TO  MCP-WIDGET
           WHEN    031
               MOVE    "KGKETACHK"     TO  MCP-WIDGET
           WHEN    201
               MOVE    "B01"           TO  MCP-WIDGET
           WHEN    202
               MOVE    "B02"           TO  MCP-WIDGET
           WHEN    206
               MOVE    "B06"           TO  MCP-WIDGET
           WHEN    207
               MOVE    "B07"           TO  MCP-WIDGET
           WHEN    208
               MOVE    "B08"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
      *    入力箇所からカーソルをセット
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SELNUM"
               MOVE    002             TO  SPA-GMN-CUR
           WHEN    "HKNJANUM"
               MOVE    003             TO  SPA-GMN-CUR
           WHEN    "JYKNKBN"
               MOVE    004             TO  SPA-GMN-CUR
           WHEN    "HKNNUM"
               MOVE    005             TO  SPA-GMN-CUR
           WHEN    "KGCHKPTN"
               MOVE    008             TO  SPA-GMN-CUR
           WHEN    "KGSUJI"
               MOVE    009             TO  SPA-GMN-CUR
           WHEN    "KGKANJI"
               MOVE    010             TO  SPA-GMN-CUR
           WHEN    "KGABT"
               MOVE    011             TO  SPA-GMN-CUR
           WHEN    "KGHYPN"
               MOVE    012             TO  SPA-GMN-CUR
           WHEN    "KGKAKKO"
               MOVE    013             TO  SPA-GMN-CUR
           WHEN    "KGETC"
               MOVE    014             TO  SPA-GMN-CUR
           WHEN    "KGKANJINASHI"
               MOVE    031             TO  SPA-GMN-CUR
           WHEN    "KGKETACHK"
               MOVE    030             TO  SPA-GMN-CUR
           WHEN    "KGKETAMIN"
               MOVE    015             TO  SPA-GMN-CUR
           WHEN    "NM1SUJI"
               MOVE    016             TO  SPA-GMN-CUR
           WHEN    "NM1KANJI"
               MOVE    017             TO  SPA-GMN-CUR
           WHEN    "NM1ABT"
               MOVE    018             TO  SPA-GMN-CUR
           WHEN    "NM1HYPN"
               MOVE    019             TO  SPA-GMN-CUR
           WHEN    "NM1KAKKO"
               MOVE    020             TO  SPA-GMN-CUR
           WHEN    "NM1ETC"
               MOVE    021             TO  SPA-GMN-CUR
           WHEN    "NMSUJINASHI"
               MOVE    029             TO  SPA-GMN-CUR
           WHEN    "NMKETACHK"
               MOVE    028             TO  SPA-GMN-CUR
           WHEN    "NMKETAMIN"
               MOVE    022             TO  SPA-GMN-CUR
           WHEN    "NM2SUJI"
               MOVE    023             TO  SPA-GMN-CUR
           WHEN    "NM2KANJI"
               MOVE    024             TO  SPA-GMN-CUR
           WHEN    "NM2ABT"
               MOVE    025             TO  SPA-GMN-CUR
           WHEN    "NM2HYPN"
               MOVE    026             TO  SPA-GMN-CUR
           WHEN    "NM2KAKKO"
               MOVE    027             TO  SPA-GMN-CUR
           WHEN    "NM2ETC"
               MOVE    006             TO  SPA-GMN-CUR
      *
           WHEN    "STYUKYMD"
               MOVE    007             TO  SPA-GMN-CUR
           WHEN    "EDYUKYMD"
               IF    ( SPA-GMN-SELNUM  =   ZERO )
                   MOVE    206         TO  SPA-GMN-CUR
               ELSE
                   MOVE    207         TO  SPA-GMN-CUR
               END-IF
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC           SECTION.
      *
           IF  ( SPA-GMN-CUR           =   206 )
               IF  ( W63-B06-STATE     =   WIDGET-INSENSITIVE )
                   MOVE    207         TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF  ( SPA-GMN-CUR           =   30 )
               IF  ( W63-KGKETAMIN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    15          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF  ( SPA-GMN-CUR           =   28 )
               IF  ( W63-NMKETAMIN-STATE
                                       =   WIDGET-INSENSITIVE )
                   MOVE    22          TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
      *    エラーメッセージセット
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "選択番号入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "保険者番号入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "一致条件入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "保険の種類入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "記号抜けチェック区分入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "有効期間開始日入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "有効期間終了日入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "番号桁数の上限が下限を下回っています"
                                           TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "選択番号を入力してください"
                                           TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "有効開始日付より前の日付になっています"
                                           TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "対象のデータが選択されています"
                                           TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "有効期間の重複するデータが存在します"
                                           TO  SPA-ERRMSG
           WHEN    "1005"
               MOVE    "追加可能最大件数に達しました"
                                           TO  SPA-ERRMSG
           WHEN    "1006"
               MOVE    "該当するデータが既に存在します"
                                           TO  SPA-ERRMSG
           WHEN    "1007"
               MOVE   "保険者番号を入力するか、指定なしを選択してくだい"
                                           TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "該当する保険者が存在しません"
                                           TO  SPA-ERRMSG
           WHEN    "1009"
               MOVE    "保険の種類を入力してください"
                                           TO  SPA-ERRMSG
           WHEN    "1010"
               MOVE    "チェックする記号桁数を入力してください"
                                           TO  SPA-ERRMSG
           WHEN    "1011"
               MOVE    "チェックする番号桁数を入力してください"
                                           TO  SPA-ERRMSG
           WHEN    "4001"      THRU        "4999"
               MOVE    "更新処理に失敗しました"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  WERR
           MOVE    SPA-ERRCD       TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  WERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "W63"           TO  SPA-MOTOPG
           MOVE    "WERR"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "WERR"          TO  MCP-WINDOW
      *
      *    ＰＵＴ　処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示処理
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
      *    確認メッセージセット
           EVALUATE    SPA-WIDCD
           WHEN    "3001"
               MOVE    "削除します。よろしいですか"
                                   TO  WRK-WIDMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE           TO  WID1
           INITIALIZE                  WID1
           MOVE    SPA-WIDCD       TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG      TO  WID1-ID1MSG
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "W63"           TO  SPA-MOTOPG
           MOVE    "WID1"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "WID1"          TO  MCP-WINDOW
      *
      *    ＰＵＴ　処理
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC             SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       800-HIZUKE-CHK-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-HENYMDG
                                       WRK-SYMD
      *
           MOVE    ZERO            TO  FLG-HIZUKE
      *
           IF    ( WRK-YMD             =   "00000000" )
             OR  ( WRK-YMD             =   "99999999" )
               MOVE    WRK-YMD     TO  WRK-HENYMDG
                                           WRK-SYMD
           ELSE
      *
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-YMD     TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF    ( SGDAY-RC            =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               ELSE
                   MOVE    1           TO  FLG-HIZUKE
               END-IF
      *
           END-IF
      *
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    半角ェック
      *****************************************************************
       800-HANKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-KANACHK
      *
           INITIALIZE                  ORCXKANACONVAREA
      *
           MOVE    300                 TO  KANACONV-LEN
           MOVE    1000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    112                 TO  KANACONV-CHAR-TYPE
           MOVE    WRK-KANACHK-DATA    TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           MOVE    KANACONV-RETURN     TO  FLG-KANACHK
           MOVE    KANACONV-OUTDATA    TO  WRK-KANACHK-DATA
           MOVE    KANACONV-MAXLEN     TO  WRK-KANACHK-MAXLEN
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険番号情報検索処理(KEY3)
      *****************************************************************
       900-HKNNUM-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
      *
           INITIALIZE                      HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SPA-NAI-STYUKYMD    TO  HKN-TEKSTYMD
                                           HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC                 =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
               INITIALIZE                  HKNNUM-REC
           END-IF
      *
           .
       900-HKNNUM-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号情報FETCH処理(KEY3)
      *****************************************************************
       900-HKNNUM-KEY3-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF ( MCP-RC                 =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
               INITIALIZE                  HKNNUM-REC
           END-IF
      *
           .
       900-HKNNUM-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険番号情報検索処理(KEY)
      *****************************************************************
       900-HKNNUM-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
      *
           INITIALIZE                      HKNNUM-REC
           MOVE    SPA-HOSPNUM          TO  HKN-HOSPNUM
           MOVE    CK4-1102-HKNNUM     TO  HKN-HKNNUM
           MOVE    CK4-1102-STYUKYMD   TO  HKN-TEKSTYMD
                                           HKN-TEKEDYMD
           MOVE    "00"                TO  HKN-PAYKBN
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC                 =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
               INITIALIZE                  HKNNUM-REC
           END-IF
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号情報検索処理(KEY)
      *****************************************************************
       900-HKNJAINF-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNJAINF
      *
           INITIALIZE                      HKNJAINF-REC
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    WRK-HKNJANUM        TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC                 =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNJAINF-REC
           ELSE
               MOVE    1               TO  FLG-HKNJAINF
               INITIALIZE                  HKNJAINF-REC
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNJAINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理(KEY)
      *****************************************************************
       900-CHK004-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM         TO  CHK004-HOSPNUM
           MOVE    CHK004-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC                 =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK004-REC
           ELSE
               MOVE    1               TO  FLG-CHK004
               INITIALIZE                  CHK004-REC
           END-IF
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK004-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理(KEY3)
      *****************************************************************
       900-CHK004-KEY3-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM         TO  CHK004-HOSPNUM
           MOVE    CHK004-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC                 =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK004-REC
           ELSE
               MOVE    1               TO  FLG-CHK004
               INITIALIZE                  CHK004-REC
           END-IF
      *
           .
       900-CHK004-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理(KEY3)
      *****************************************************************
       900-CHK004-KEY3-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK004
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF ( MCP-RC                 =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK004-REC
           ELSE
               MOVE    1               TO  FLG-CHK004
               INITIALIZE                  CHK004-REC
           END-IF
      *
           .
       900-CHK004-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理(KEY3)
      *****************************************************************
       900-CHK004-KEY9-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM         TO  CHK004-HOSPNUM
           MOVE    "%"                 TO  CHK004-KBNCD
           MOVE    "99999999"          TO  CHK004-STYUKYMD
           MOVE    "00000000"          TO  CHK004-EDYUKYMD
           MOVE    CHK004-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC                 =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK004-REC
           ELSE
               MOVE    1               TO  FLG-CHK004
               INITIALIZE                  CHK004-REC
           END-IF
      *
           .
       900-CHK004-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理(KEY9)
      *****************************************************************
       900-CHK004-KEY9-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK004
      *
           MOVE    "tbl_chk004"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF ( MCP-RC                 =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK004-REC
           ELSE
               MOVE    1               TO  FLG-CHK004
               INITIALIZE                  CHK004-REC
           END-IF
      *
           .
       900-CHK004-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
