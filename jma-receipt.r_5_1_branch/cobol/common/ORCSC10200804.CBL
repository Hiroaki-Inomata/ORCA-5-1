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
       PROGRAM-ID.                 ORCSC10200804.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（診療）
      *                      Ｈ２０年４月改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/03/09    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC−多々納  01/04/19  時間外加算の乳幼児加算の修正等
      * 02.00.00     MCC−多々納  01/08/09  全体の修正
      * 02.00.01     MCC−多々納  02/03/01  在宅薬剤、在宅器材の回数を１
      *                                     回のみへ
      * 02.00.02     MCC−多々納  02/04/02  慢性疼痛疾病管理料が有る時
      *                                     外来管理加算を削除する
      * 02.00.03     MCC-多々納   02/04/10  労災用追加
      * 02.00.04     NACL-多々納  02/08/09  労災チェック追加
      * 02.00.05     NACL-多々納  02/10/02  注加算チェックの追加
      *                                     （すべての診区のチェック）
      * 02.00.06     NACL−多々納 02/10/10  外来管理加算チェックの有無
      * 02.00.07     NACL−多々納 02/10/30  特定薬剤治療管理料の重複なし
      * 02.00.08     NACL−多々納 02/10/31  外来管理加算自動算定なしの追加
      * 02.00.09     NACL−多々納 02/11/11  ＤＵＭＭＹ診察料追加
      * 02.00.10     NACL−多々納 02/11/25  ＤＵＭＭＹ診察料のチェックを
      *                                     Ｋ０２のみとする
      * 02.00.11     NACL−多々納 02/12/06  初診の時労災の置換えをしない
      * 02.00.12     NACL-多々納  02/12/17  在宅指導料の追加（143)
      * 02.00.13     NACL-多々納  03/03/14  特定薬剤治療管理算定有無追加
      * 02.00.13     NACL-多々納  03/04/04  在宅加算料の算定チェック修正
      * 02.00.14     NACL-多々納  03/05/06  １月後の判定修正
      * 02.00.16     NACL-多々納  03/06/17  診療情報提供料重複ＯＫ
      * 02.00.17     NACL-多々納  03/07/17  算定履歴検索修正他
      * 02.00.18     NACL-多々納  03/08/29  Ｈ１５．９から
      *                                     労災・自賠責逓減制の廃止
      *                                     （レセ電コードの変更なし）
      * 02.00.19     NACL-多々納  05/01/27  酸素追加
      * 02.00.20     NACL-多々納  05/11/21  開放型病院共同指導料チェック追加
      *                                     MONFUNC 対応 他
      * 02.00.21     NACL-多々納  05/12/08  難病外来指導管理料等の初診算定日判定修正
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      * 03.05.01     NACL-多々納  07/08/22  加算順チェック追加
      * 04.00.00     NACL-多々納  07/09/XX  テーブル４００対応
      * 04.01.00     NACL-多々納  07/10/26  悪性腫瘍特異物資治療管理コメント対応
      * 04.02.00     NACL-多々納  08/03/XX  Ｈ２０年４月改正対応
      * 04.03.00     NACL=多々納  08/07/17  在宅末期医療総合診療料複数回数可
      * 04.03.00     NACL=多々納  08/08/07  労災初診料Ｈ２０年４月改正対応他
      *****************************************************************
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
           03  FLG-CHK             PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
      *
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-SP              PIC 9(01).
      *
           03  FLG-KYUJITU         PIC 9(01).
           03  FLG-GAIKAN          PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *    電話再診有無
           03  FLG-TELSAI          PIC 9(01).
      *    特定薬剤治療管理初回
           03  FLG-SYOKAI          PIC 9(01).
           03  FLG-SYOKAI2         PIC 9(01).
           03  FLG-SYOKAI3         PIC 9(01).
      *    指導料
           03  FLG-SIDOU           PIC 9(01).
      *
      *    外来管理加算
           03  FLG-GAIRAI          PIC 9(01).
      *    継続加算
           03  FLG-KEIZOK          PIC 9(01).
      *    労災加算
           03  FLG-RSIFLG          PIC 9(01).
      *
           03  FLG-ADD             PIC 9(01).
      *    他保険算定済み
           03  FLG-TAHOKEN         PIC 9(01).
      *    他科算定済み
           03  FLG-TAKA            PIC 9(01).
      *    ＤＵＭＭＹ診察料用
           03  FLG-KENSIN          PIC 9(01).
           03  FLG-DUMMY           PIC 9(01).
           03  FLG-DUMMY-ARI       PIC 9(01).
           03  FLG-DELETE          PIC 9(01).
      *
           03  FLG-SINSATU         PIC 9(01).
      *
           03  FLG-JIKASAN         PIC 9(01).
           03  FLG-TAIIN           PIC 9(01).
           03  FLG-SYOHEN           PIC 9(01).
      *
           03  FLG-TBL             PIC 9(01).
           03  FLG-DEL             PIC 9(01).
           03  FLG-TIME-NYU        PIC 9(01).
           03  FLG-DOUJITU        PIC 9(01).
      *    同日初診チェック
           03  FLG-TAKA-2          PIC 9(01).
           03  FLG-TAKA-3          PIC 9(01).
           03  FLG-SINSATU2        PIC 9(01).
           03  FLG-KIJUN-ERR       PIC 9(01).
      *
           03  FLG-NYUIN           PIC 9(01).
      *    麻薬判定
           03  FLG-KANWA           PIC 9(01).
           03  FLG-MAYAKU          PIC 9(01).
      *
           03  FLG-SANTEI-ARI      PIC 9(01).
           03  FLG-FUKUSU          PIC 9(01).
      *    他医院退院日
           03  FLG-TAITAIIN        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-Z2              PIC 9(04).
           03  IDX-Z3              PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-T               PIC 9(04).
      *
           03  IDX-MAX             PIC 9(04).
           03  IDX-STR             PIC 9(04).
      *
           03  IDX-ZAI             PIC 9(04).
           03  IDX-ZAI1            PIC 9(04).
           03  IDX-ZAI2            PIC 9(04).
           03  IDX-SYU             PIC 9(04).
           03  IDX-SYU2            PIC 9(04).
           03  IDX-GAIKAN          PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-2               PIC 9(02).
      *    診察料の位置
           03  IDX-SINSATU          PIC 9(04).
      *    注加算位置
           03  IDX-CHU              PIC 9(04).
      *
           03  IDY                  PIC 9(04).
      *    DUMMY診察料チェック
           03  IDX-MAE-IDX          PIC 9(04).
           03  IDX-DUMMY            PIC 9(04).
      *
           03  IDX-N                PIC 9(04).
           03  IDX-SINSATU2         PIC 9(04).
      *
           03  IDX-ERR                 PIC 9(04).
           03  IDX-ECHK                PIC 9(04).
      *
           03  IDX-SN               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *    パス名
           03  WRK-DBPATH          PIC X(64).
      *
           03  WRK-IDX             PIC 9(03).
      *
           03  WRK-MOJI            PIC X(01). 
      *    加算エラー分
           03  WRK-KASAN-NASI      PIC X(01). 
      *
           03  WRK-ERRCD           PIC X(04). 
      *
      *    労災救急医療管理加算有無
           03  WRK-RSISYOSIN       PIC 9(04).
      *
           03  WRK-TENSUKEI        PIC 9(08)V99.
           03  WRK-TENSUKEI-9      PIC 9(08).
           03  WRK-TENSUKEI-G      PIC 9(08).
           03  WRK-TENSUKEI-GEN    PIC 9(08).
      *
           03  WRK-TENSU-KEISAN.
      *        往診時、所定点数加算分
               05  WRK-TENSU-O1        PIC 9(08)V99.
      *        往診時、所定点数加算外
               05  WRK-TENSU-O2        PIC 9(08)V99.
      *        往診以外
               05  WRK-TENSU-GAI       PIC 9(08)V99.
               05  WRK-TENSU-KEI       PIC 9(08)V99.
      *
           03  WRK-TENSU1          PIC 9(08).
      *
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-KEIYMD.
               05  WRK-KEI-YY          PIC 9(04).
               05  WRK-KEI-MM          PIC 9(02).
               05  WRK-KEI-DD          PIC 9(02).
           03  WRK-KEIYMDZEN           PIC X(08).
      *----(01.00.01) LINE ADD START ----------------------------------
      *    乳幼児の自動加算コード
           03  WRK-NYU-SRYCD           PIC X(09).
      *    入力時表示用
           03  IDX-INCNT               PIC 9(04).
           03  WRK-HEN-INPUTCD         PIC X(22).
      *    労災用、外来管理読み替え算定
           03  WRK-GAI-CNT             PIC 9(04).
           03  WRK-GAI-IDX             PIC 9(04).
           03  WRK-GAI-TEN             PIC 9(08).
      *
           03  WRK-SYOKAI-YMD3.
               05  WRK-SYO-YY3         PIC 9(04).
               05  WRK-SYO-MM3         PIC 9(02).
               05  WRK-SYO-DD3         PIC 9(02).
      *
           03  WRK-SYOKAI-YMD          PIC X(08).
           03  WRK-SANTEI-CNT          PIC 9(04).
      *    特定薬剤初回算定日
           03  WRK-SANTEI-YMD          PIC X(08).
           03  WRK-COM-YM              PIC X(06).
           03  WRK-WYM.
               05  WRK-WYY             PIC X(02).
               05  WRK-WMM             PIC X(02).
           03  WRK-SYOKAI              PIC 9(04).
      *
      *    診察料算定用
           03  WRK-KBN                 PIC X(01).
           03  WRK-KBN2                PIC X(01).
           03  WRK-KAISU               PIC 9(04).
           03  WRK-SAN-KAISU           PIC 9(04).
           03  WRK-HOSPSBT             PIC 9(01).
           03  WRK-SINSATU-KBN         PIC X(01).
           03  WRK-SINSATU-KBN2        PIC X(01).
           03  WRK-SINSATU-KAISU       PIC 9(01).
      *    診察料チェック用
           03  WRK-KEICD               PIC X(04).
           03  WRK-KEICD-IDX           PIC 9(04).
      *厚生大臣が定める患者
           03  WRK-TOKUTEI-KNJ       PIC  9(01).
      *人工腎臓
           03  WRK-TOKUTEI-JNZ       PIC  9(01).
      *厚生大臣が定める患者（外来管理）
           03  WRK-TOKUTEIG-KNJ      PIC  9(01).
      *
      *    初診算定日
           03  WRK-SYOYMD              PIC X(08).
           03  WRK-SYOYMD-K            PIC X(08).
      *    入院日＋７日
           03  WRK-NYUINYMD-7           PIC X(08).
      *
      *    重複警句エラー用
           03  IDX-KEI                 PIC 9(04).
           03  FLG-KEI                 PIC 9(01).
      *    年齢判定用
           03  WRK-KGNAGE              PIC 9(03).
           03  WRK-JGNAGE              PIC 9(03).
           03  FLG-NENREI-OK           PIC 9(01).
      *    小児科外来診察料加算判定
           03  WRK-SYONIKA-KBN         PIC X(01).
      *
           03  WRK-INPUTCD             PIC X(22).
      *    ニコチンチェック
           03  WRK-SYO-YMD.
               05  WRK-SYO-YY          PIC 9(04).
               05  WRK-SYO-MM          PIC 9(02).
               05  WRK-SYO-DD          PIC 9(02).
           03  WRK-SYO-YMD-12          PIC X(08).
           03  FLG-SANTEI-OK           PIC 9(01).
           03  IDX-TEI                 PIC 9(04).
           03  WRK-TEI-CNT             PIC 9(01).
      *    小児カウンセリング料
           03  WRK-LST-YMD.
               05  WRK-LST-YY          PIC 9(04).
               05  WRK-LST-MM          PIC 9(02).
               05  WRK-LST-DD          PIC 9(02).
      *    悪性マーカ初回月チェック
           03  WRK-AKU-YMD.
               05  WRK-AKU-YY          PIC 9(04).
               05  WRK-AKU-MM          PIC 9(02).
               05  WRK-AKU-DD          PIC 9(02).
           03  WRK-AKUSEI-YMD          PIC X(40).
      *!
      *    酸素
           03  IDX-SANSO               PIC 9(04).
      *
           03  WRK-SRYKA               PIC X(02).
      *    悪性腫瘍位置
           03  WRK-AKUSEI-IDX          PIC 9(04).
           03  WRK-NAME                PIC X(80).
           03  WRK-MEISYO-G            PIC X(80).
      *    麻薬判定
           03  WRK-DD                  PIC 9(02).
           03  IDX-N2                  PIC 9(02).
      *    入院算定上限
           03  WRK-NYUCNT              PIC 9(03).
      *
      *    保存用ＳＰＡ
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *    保存用ＳＰＡ
       01  HZN2-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN2-//.
      *
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    特定薬剤管理加算料テーブル
       01  TBL-TOKUYAKU-AREA.
           03  FILLER              PIC X(09)   VALUE   "113000410".
           03  FILLER              PIC X(09)   VALUE   "180000110".
           03  FILLER              PIC X(09)   VALUE   "180000210".
           03  FILLER              PIC X(09)   VALUE   "113000510".
       01  TBL-TOKUYAKU-AREA-R     REDEFINES   TBL-TOKUYAKU-AREA.
           03  TBL-TOKUYAKU-OCC    OCCURS   4  INDEXED  TBL-TOK.
               05  TBL-TOKSRYCD    PIC X(09).
      *
      *    診察料テーブル
       01  TBL-SINSATU-AREA.
      *    初診料
           03  FILLER              PIC X(14)   VALUE   "100F 111000110".
      *    同日初診
           03  FILLER              PIC X(14)   VALUE   "100FD111011810".
      *
      *    再診料（一般 病院）
           03  FILLER              PIC X(14)   VALUE   "101  112007410".
      *    電話再診料（一般 病院）
           03  FILLER              PIC X(14)   VALUE   "101T 112007950".
      *    同日再診（一般 病院）
           03  FILLER              PIC X(14)   VALUE   "101D 112008350".
      *    電話同日再診（一般 病院）
           03  FILLER              PIC X(14)   VALUE   "101S 112008850".
      *
      *    再診料（一般 診療所）
           03  FILLER              PIC X(14)   VALUE   "102  112009210".
      *    電話再診（一般 診療所）
           03  FILLER              PIC X(14)   VALUE   "102T 112009750".
      *    同日再診（一般 診療所）
           03  FILLER              PIC X(14)   VALUE   "102D 112010150".
      *    電話同日再診（一般 診療所）
           03  FILLER              PIC X(14)   VALUE   "102S 112010650".
      *
      *    小児科外来診察料(院外処方 初診)
           03  FILLER              PIC X(14)   VALUE   "200FG113003510".
      *    小児科外来診察料(院内処方 初診)
           03  FILLER              PIC X(14)   VALUE   "200FN113003710".
      *    小児科外来診察料(院外処方 再診)
           03  FILLER              PIC X(14)   VALUE   "200 G113003610".
      *    小児科外来診察料(院内処方 再診)
           03  FILLER              PIC X(14)   VALUE   "200 N113003810".
      *
      *
      *    外来診療料
           03  FILLER              PIC X(14)   VALUE   "301  112011310".
      *    同日外来診療料
           03  FILLER              PIC X(14)   VALUE   "301D 112011710".
      *
       01  TBL-SINSATU-AREAR       REDEFINES   TBL-SINSATU-AREA.
           03  TBL-SINSATU-OCC         OCCURS  16    INDEXED  TBL-IDX.
      *        １：一般、２：小児科外来診察料、３：外来診療料
               05  TBL-KAISU           PIC 9(01).
      *        老人一般区分（０：一般、１：老人）(０：なし）
               05  TBL-ROUJIN          PIC 9(01).
      *        病院種別（１：病院、２：診療所）
               05  TBL-HOSPSBT         PIC 9(01).
      *        D:同日再診、T：電話再診、S:同日電話再診、F:初診
               05  TBL-KBN             PIC X(01).
      *        G:院外、N:院内（小児科外来診察料）
      *        D:同日初診
               05  TBL-KBN2            PIC X(01).
      *        診療コード
               05  TBL-SRYCD           PIC X(09).
      *
       01  TBL-SIN-MAX                 PIC 9(04)   VALUE   16.
      *
      *    外来管理加算・継続管理加算テーブル
       01  TBL-GAIRAI-AREA.
      *    一般
           03  TBLG-SRYCD          PIC X(09)   VALUE   "112011010".
      ***  03  FILLER              PIC X(13)   VALUE   "100G112011010".
      *
      **** 老人 病院
      **   03  FILLER              PIC X(13)   VALUE   "111G112705110".
      *
      *    老人 診療所
      **** 03  FILLER              PIC X(13)   VALUE   "112G112705310".
      *
      *D   継続管理加算（一般）
      *    03  FILLER              PIC X(13)   VALUE   "000K112007170".
      *    継続管理加算（老人）
      *D   03  FILLER              PIC X(13)   VALUE   "010K112702270".
      *
      *
      *01  TBL-GAIRAI-AREAR       REDEFINES   TBL-GAIRAI-AREA.
      *    03  TBL-GAIRAI-OCC          OCCURS  1    INDEXED  TBLG-IDX.
      *        回数
      *        05  TBLG-KAISU           PIC 9(01).
      *        老人一般区分（０：一般、１：老人）
      *        05  TBLG-ROUJIN          PIC 9(01).
      *        病院種別（１：病院、２：診療所）
      *        05  TBLG-HOSPSBT         PIC 9(01).
      *         :外来管理加算、K:継続管理加算
      *        05  TBLG-KBN             PIC X(01).
      *        診療コード
      ******** 05  TBLG-SRYCD           PIC X(09).
      *
      *    労災診察料テーブル
       01  TBL-RSI-SINSATU-AREA.
      *    初診料
           03  FILLER              PIC X(13)   VALUE   "0 F 101110010".
      *TE  同日初診
           03  FILLER              PIC X(13)   VALUE   "0 FD101110040".
      *    再診料
           03  FILLER              PIC X(13)   VALUE   "0 S 101120040".
      *
      *    外来管理加算
           03  FILLER              PIC X(13)   VALUE   "0 G 101120070".
      *
       01  TBL-RSI-SINSATU-AREAR   REDEFINES   TBL-RSI-SINSATU-AREA.
           03  TBL-RSI-SINSATU-OCC     OCCURS  4    INDEXED  TBL-RSI.
      *        回数
               05  TBLR-KAISU           PIC 9(01).
               05  FILLER               PIC X(01).
      *        F:初診、S:再診、G:外来管理加算
               05  TBLR-KBN             PIC X(01).
      *        D:同日初診
               05  TBLR-KBN2            PIC X(01).
      *        診療コード
               05  TBLR-SRYCD           PIC X(09).
      *
      *    小児科外来診察料テーブル
           COPY   "CMSYONIKATBL.INC".
      *
      *    酸素コードテーブル
           COPY     "CMSANSOTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    診療行為算定編集パラメタ
           COPY    "CPORCSC90TEN.INC".
      *
      *    診療行為算定歴チェックパラメタ
           COPY    "CPORCSC91.INC".
      *
      *    診療行為点数編集・基本チェック
           COPY    "CPORCSC92.INC".
      *
      *    画面編集
           COPY    "CPORCSC94.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC00AREA
           WRK-SCR-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           IF      LNK-ORCSC-PG        =   "K02"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           ELSE
               INITIALIZE                      SPA-K01KYOUTU
           END-IF
      *
           EVALUATE    LNK-ORCSC-KBN
      *        剤別　チェック、編集、自動算定
               WHEN    "1"
                   PERFORM 100-SEL-SYORI-SEC
      *        全体のチェック
               WHEN    "2"
                   PERFORM 200-ALL-SYORI-SEC
      *        自動算定
               WHEN    "3"
                   PERFORM 300-AUTO-SYORI-SEC
               WHEN    OTHER
                   MOVE    "1001"          TO  SPA-ERRCD
           END-EVALUATE
      *
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    剤別のチェック、編集、自動算定処理
      *****************************************************************
       100-SEL-SYORI-SEC                SECTION.
      *
      *    チェック、自動算定
           PERFORM 1001-NAI-CHK-SEC
           IF     (WRK-GMN-INPUTCD(IDX-MAX)(1:1)
                                       =   "."   )  OR
                  (WRK-GMN-INPUTCD(IDX-MAX)
                                       =   SPACE )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               CONTINUE
           ELSE
      *
      *        点数計算
               PERFORM 1002-TENSU-KEI-SEC
           END-IF
      *
           .
       100-SEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       1001-NAI-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-SYU
           INITIALIZE                  WRK-AREA
           MOVE    SPACE               TO  WRK-NYU-SRYCD
           MOVE    ZERO                TO  WRK-SINSATU-KAISU
      *
           MOVE    ZERO                TO  FLG-TIME-NYU
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE      ) OR
                             ((WRK-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (WRK-COM-INPUTCD (IDX-Z) =   SPACE ))
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "." OR "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
      *                ＤＵＭＭＹ診察料判定
                       PERFORM 10012-DUMMYSYORI-SEC
      ************ WHEN    "8"
                   WHEN    "."
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    "1"
      *                手技料
                       PERFORM 10011-SYUGISYORI-SEC
      *
                   WHEN    "8"
      *                コメントのチェック
                       PERFORM 10018-COMMENT-SEC
      *
                   WHEN    "6"
      *                在宅は可能
                       IF      WRK-COM-SRYKBN (IDX-Z)  =   "14"
                           CONTINUE
                       ELSE
                           MOVE    "0147"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                       END-IF
                   WHEN    "7"
      *!               在宅は可能
                     IF    WRK-COM-SRYKBN (IDX-Z)  =   "14"
                       PERFORM 10017-KIZAI-CHK-SEC
                     ELSE
                       IF      WRK-COM-DATAKBN (IDX-Z) =   "3"
      *                    フィルムの時エラー
                           MOVE    "0047"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                       END-IF
      *
      *                酸素・窒素はエラー
                       IF      WRK-COM-TOKUKIZAISBT1 (IDX-Z) =   2
                           MOVE    "0039"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR  (IDX-Z)
                       END-IF
                     END-IF
               END-EVALUATE
      *
      *        初診、再診、指導、在宅の時、回数は１のみ
      *        IF     (WRK-COM-SRYKBN (IDX-Z)  =   "11"  OR
      *                                            "12"  OR
      *                                            "13"  OR
      *                                            "14" )
      *           AND (WRK-ERRCD               =   SPACE )
      *           AND (WRK-GMN-INPUTCD (IDX-Z)(1:1)
      *                                    NOT =   "."  )
      *?????          (WRK-COM-SRYKBN (IDX-Z)  =   "14" )
      *??????     AND  WRK-COM-SRYSYUKBN(IDX-Z)  = "140" )
      *?????      IF      (WRK-COM-KAIFLG (IDX-Z)  =   1  )  AND
      *           IF      (WRK-COM-KAISU(IDX-Z)    >   1  )
      *                PERFORM 10012-KAISU-CHK-SEC
      *            END-IF
      *******  END-IF
           END-PERFORM
      *    初診、再診、指導、在宅の時、回数は１のみ
           IF     (WRK-ERRCD           =   SPACE)
               IF     (WRK-COM-SRYKBN (IDX-MAX)  =   "11"  OR
                                                   "12"  OR
                                                   "13"  OR
                                                   "14" )
                  IF      (WRK-COM-KAISU(IDX-MAX)    >   1  )
                       PERFORM 10012-KAISU-CHK-SEC
                   END-IF
               END-IF
           END-IF
      *
      *    年齢加算算定チェック
           IF      FLG-TIME-NYU        =   ZERO
              IF     (WRK-ERRCD            =   SPACE)   AND
                     (IDX-SYU               >   ZERO )
                   PERFORM 1007-NENREI-SYORI-SEC
               END-IF
           END-IF
      *
      *    酸素の補正率を自動発生する
           IF     (WRK-ERRCD           =   SPACE)   AND
                  (IDX-SANSO           >   ZERO )
               PERFORM 10011-SANSO-ADD-SEC
           END-IF
      *
      *    注加算コードの順チェック
           IF     (WRK-ERRCD           =   SPACE)   AND
                  (IDX-SYU             >   ZERO )
               PERFORM 10012-CHUKSNCD-CHK-SEC
           END-IF
      *
           MOVE    WRK-ERRCD           TO  SPA-ERRCD
      *
           .
       1001-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数チェック処理
      *****************************************************************
       10012-KAISU-CHK-SEC            SECTION.
      *
      *    診療情報提供料（１）は重複ＯＫとする
           IF     (IDX-SYU             >   ZERO )  AND
                 ((WRK-COM-INPUTCD (IDX-SYU)   =   "180016110"
      *??                                      OR  "113009510"
      *        傷病手当意見書交付料
                                               OR  "180000710"
      *        療養費同意書交付料
                                               OR  "113004310"
      *        退院前訪問指導料
                                               OR  "190030310"
      *        退院時診療状況添付加算
                                               OR  "113009470" )
      *        血糖自己測定器加算
             OR  ((WRK-COM-CDKBN-KBN (IDX-SYU) =   "C"  )  AND
                  (WRK-COM-CDKBN-KBNNUM (IDX-SYU)  =   "150" ))
      *        在宅末期医療総合診療料
             OR  ((WRK-COM-CDKBN-KBN (IDX-SYU) =   "C"  )  AND
                  (WRK-COM-CDKBN-KBNNUM (IDX-SYU)  =   "003" )))
               CONTINUE
           ELSE
               MOVE    "0008"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-MAX)
           END-IF
           .
       10012-KAISU-CHK-EXT.
           EXIT.
      *!
      *****************************************************************
      *    器材チェック処理
      *****************************************************************
       10017-KIZAI-CHK-SEC            SECTION.
      *
           IF      WRK-COM-SANSOKBN (IDX-Z)    =   1
      *        酸素の時、酸素補正率を自動加算
               MOVE    WRK-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
               PERFORM 100115-SANSO-CHK-SEC
               IF      FLG-SANSO-OK        =   1
                   MOVE    IDX-Z           TO  IDX-SANSO
               ELSE
                   IF      IDX-SANSO        =   ZERO
                       MOVE    "0038"       TO  WRK-ERRCD
                       MOVE    "1"          TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *
           .
       10017-KIZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素判定処理
      *****************************************************************
       100115-SANSO-CHK-SEC           SECTION.
      *
      *
           SET     TBL-SANSO           TO  1
           SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                   AT  END
                       MOVE    ZERO            TO  FLG-SANSO-OK
                   WHEN    WRK-SRYCD       =   TBL-SANSO-SRYCD
                                                           (TBL-SANSO)
                       MOVE    1               TO  FLG-SANSO-OK
           END-SEARCH 
      *
           .
       100115-SANSO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    酸素補正率自動加算処理
      *****************************************************************
       10011-SANSO-ADD-SEC                  SECTION.
      *
           MOVE    "770020070"         TO  WRK-SRYCD
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   SPA-MAX-LINE )  OR
                              (WRK-COM-INPUTCD(IDX1)   =   SPACE AND
                               WRK-GMN-INPUTCD(IDX1)   =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                   MOVE    IDX1                TO  IDX-T
                   MOVE    1                   TO  FLG-ARI
               END-IF
           END-PERFORM
           IF      FLG-ARI             =   ZERO
               MOVE    IDX-SANSO           TO  IDX-S
           END-IF
      *    酸素補正率自動加算
           PERFORM 1100-KASAN-SET-SEC
      *
           .
       10011-SANSO-ADD-EXT.
           EXIT.
      *!
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       10011-SYUGISYORI-SEC            SECTION.
      *
      *    診察料の判定
           PERFORM 100110-SINSATU-CHK-SEC
      *
           MOVE    ZERO                 TO  FLG-DEL
           EVALUATE    TRUE
      *        在宅診察料が有る時
               WHEN    SPA-HOUMON-ARI      =   1
                   PERFORM 100111-HOUMON-CHK-SEC
      *        在宅末期医療総合診察料
               WHEN    SPA-ZAIMATU-ARI NOT =   ZERO
                   PERFORM 100111-HOUMON-CHK-SEC
      *
      *        開放型病院共同指導料（１）等の判定
               WHEN    SPA-KAIHOU1-ARI     =   1
                   PERFORM 100112-KAIHOU1-CHK-SEC
      *
      *       寝たきり老人退院時共同指導料の判定
               WHEN     SPA-KAIHOU2-ARI     =   1
                   PERFORM 100113-KAIHOU2-CHK-SEC
           END-EVALUATE
      *
           IF      (FLG-DEL            =   ZERO )  AND
                   (WRK-ERRCD          =   SPACE)
               PERFORM 100114-SYUGISYORI-CHK-SEC
           END-IF
           .
       10011-SYUGISYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅診察料チェック処理
      *****************************************************************
       100110-SINSATU-CHK-SEC           SECTION.
      *    診察料セット
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-KBN
                       MOVE    SPACE           TO  WRK-KBN2
                       MOVE    ZERO            TO  WRK-KAISU
                       MOVE    ZERO            TO  FLG-TBL
                   WHEN    WRK-COM-INPUTCD(IDX-Z)
                                               =   TBL-SRYCD (TBL-IDX)
                       MOVE    TBL-KAISU (TBL-IDX) TO  WRK-KAISU
                       MOVE    TBL-KBN (TBL-IDX)   TO  WRK-KBN
                       MOVE    TBL-KBN2(TBL-IDX)   TO  WRK-KBN2
                       MOVE    1                   TO  FLG-TBL
           END-SEARCH
      *
           IF     (FLG-TBL             =   ZERO )  AND
                  (SPA-HKNKBN          =   1    )
      *    労災診察料検索
               SET     TBL-RSI              TO  1
               SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
                  AT   END
                       MOVE    SPACE               TO  WRK-KBN
                       MOVE    SPACE               TO  WRK-KBN2
                       MOVE    ZERO                TO  WRK-KAISU
                   WHEN    WRK-COM-INPUTCD(IDX-Z)
                                               =   TBLR-SRYCD (TBL-RSI)
                       MOVE    TBLR-KBN  (TBL-RSI) TO  WRK-KBN
                       MOVE    TBLR-KBN2 (TBL-RSI) TO  WRK-KBN2
                       MOVE    TBLR-KAISU(TBL-RSI) TO  WRK-KAISU
                       MOVE    1                   TO  FLG-TBL
               END-SEARCH
           END-IF
           .
       100110-SINSATU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅診察料チェック処理
      *****************************************************************
       100111-HOUMON-CHK-SEC           SECTION.
      *
           IF      FLG-TBL             =   1
               IF      WRK-KBN             =   "F"
                   MOVE    "1042"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
      *    在宅診察料が有る時、再診料を削除する
           IF      WRK-COM-SRYKBN (IDX-Z)  =   "12"
               INITIALIZE              WRK-GMN-TBLREC(IDX-Z)
               INITIALIZE              WRK-COM-TBLREC(IDX-Z)
               MOVE    1                   TO  FLG-DEL
           ELSE
               IF     (FLG-TBL             =   ZERO )  AND
                      (WRK-ERRCD           =   SPACE)
      *        初診料が算定済みであること
                   IF     (LNK-ORCSC-SYOYMD    =   SPACE ) OR
                         ((LNK-ORCSC-SYORIFLG  =   1  )  AND
                          (SPA-SYOYMD1         =   SPA-SRYYMD ))
                       MOVE    "1020"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           END-IF
           .
       100111-HOUMON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開放型病院共同指導料（１）チェック処理
      *****************************************************************
       100112-KAIHOU1-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-DEL
           IF      FLG-TBL             =   1
      *        初診料同時算定エラー
               IF     (WRK-KBN             =   "F" )
                   MOVE    "1042"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               ELSE
      *            再診料を削除する
                   INITIALIZE              WRK-GMN-TBLREC(IDX-Z)
                   INITIALIZE              WRK-COM-TBLREC(IDX-Z)
                   MOVE    1                   TO  FLG-DEL
               END-IF
           ELSE
      *        再診料を削除する
               IF      WRK-COM-SRYKBN (IDX-Z)  =   "12"
                   INITIALIZE              WRK-GMN-TBLREC(IDX-Z)
                   INITIALIZE              WRK-COM-TBLREC(IDX-Z)
                   MOVE    1                   TO  FLG-DEL
               END-IF
           END-IF
      *
           IF      (FLG-DEL            =   ZERO )  AND
                   (WRK-ERRCD          =   SPACE)
      *        初診料が算定済みであること
               IF     (LNK-ORCSC-SYOYMD    =   SPACE )
      *******    OR  ((LNK-ORCSC-SYORIFLG  =   1  )  AND
      *******         (SPA-SYOYMD1         =   SPA-SRYYMD ))
                   MOVE    "1020"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
           IF      (FLG-DEL            =   ZERO )  AND
                   (WRK-ERRCD          =   SPACE)
      *        往診料
               IF     (WRK-COM-CDKBN-KBN (IDX-Z)   =   "C" ) AND
                      (WRK-COM-CDKBN-KBNNUM(IDX-Z) =   "000" ) AND
                      (WRK-COM-DATAKBN  (IDX-Z)    =   "1"   )
                   MOVE    "1040"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
           .
       100112-KAIHOU1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    寝たきり老人退院時共同指導料チェック処理
      *****************************************************************
       100113-KAIHOU2-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-DEL
           IF      FLG-TBL             =   1
      *        初診料エラー
               IF     (WRK-KBN             =   "F" )
                   MOVE    "1042"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        再診料を削除する
               IF      WRK-COM-SRYKBN (IDX-Z)  =   "12"
                   INITIALIZE              WRK-GMN-TBLREC(IDX-Z)
                   INITIALIZE              WRK-COM-TBLREC(IDX-Z)
                   MOVE    1               TO  FLG-DEL
               END-IF
           END-IF
      *    往診料エラー
           IF      (FLG-DEL            =   ZERO )  AND
                   (WRK-ERRCD          =   SPACE)
               IF     (WRK-COM-CDKBN-KBN (IDX-Z)   =   "C" ) AND
                      (WRK-COM-CDKBN-KBNNUM(IDX-Z) =   "000" ) AND
                      (WRK-COM-DATAKBN  (IDX-Z)    =   "1"   )
                   MOVE    "1040"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
           .
       100113-KAIHOU2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料の決定処理
      *****************************************************************
       100114-SYUGISYORI-CHK-SEC           SECTION.
      *
      *    診察料の判定
           PERFORM 2001-SINSATU-CHK-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      100114-SYUGISYORI-CHK-EXT
           END-IF
      *
           IF      WRK-KAISU       NOT =   ZERO
               MOVE    WRK-KAISU       TO  WRK-SINSATU-KAISU
           END-IF
      *
      *    労災の診察料の判定
           IF      SPA-HKNKBN          =   1
      *D       IF      (SPA-RSI-HKNKBN      =   "5" )
               IF      (SPA-RSI-JUNKYO      =   "1" )
      *        自賠責の健保準拠
                   PERFORM 10013-JBI-SINSATU-CHK-SEC
               ELSE
      *        自賠責の健保準拠以外
                   PERFORM 10013-RSI-SINSATU-CHK-SEC
               END-IF
               IF      WRK-ERRCD       NOT =   SPACE
                   GO      TO      100114-SYUGISYORI-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-NYUGAIKBN       =   "2"
           AND     LNK-ORCSC-PG        =   "K02"
      *    外来管理加算、継続管理加算の判定
               PERFORM 100111-KEIZOKU-CHK-SEC
               IF      FLG-GAIRAI          =   1
      *        労災読み替え加算分は外来管理加算削除としない
                   IF      WRK-COM-RSIFLG2 (IDX-Z)     =   "1"
                       MOVE    ZERO            TO  FLG-GAIRAI
                   END-IF
               END-IF
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      100114-SYUGISYORI-CHK-EXT
            END-IF
      *
      *    電話再診が有る時、外来管理加算・継続管理加算を削除する
           IF      LNK-ORCSC-TELSAI-ARI    =   1
               IF     (FLG-GAIRAI          =   1  )  OR
                      (FLG-KEIZOK          =   1  )
                   INITIALIZE              WRK-GMN-TBLREC(IDX-Z)
                   INITIALIZE              WRK-COM-TBLREC(IDX-Z)
                   GO    TO      100114-SYUGISYORI-CHK-EXT
               END-IF
           END-IF
      *
      *    慢性疼痛疾病管理料が有る時、外来管理加算を削除する
           IF     (LNK-ORCSC-TOKUTEI-KNJ2  =   1  )  OR
      ************(LNK-ORCSC-TOKUTEI-KNJ   =   1  )  OR
                  (LNK-ORCSC-GAIRAI-NASI   =   1  )
               IF      FLG-GAIRAI          =   1
      *            訂正の時、自動削除はしない
                AND    (LNK-ORCSC-SYORIFLG   =   ZERO)
                   INITIALIZE              WRK-GMN-TBLREC(IDX-Z)
                   INITIALIZE              WRK-COM-TBLREC(IDX-Z)
                   GO    TO      100114-SYUGISYORI-CHK-EXT
               END-IF
           END-IF
      *
      *    外来管理加算・継続管理加算の剤は、１明細のみとする
           IF      WRK-KASAN-NASI      =   "1"
               IF      WRK-COM-INPUTCD(IDX-Z)(1:1) =   "1"
                   MOVE    "0059"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   GO      TO      100114-SYUGISYORI-CHK-EXT
               END-IF
           END-IF
           IF     (FLG-GAIRAI          =   1  )  OR
                  (FLG-KEIZOK          =   1  )
               MOVE    "1"                 TO  WRK-KASAN-NASI
           END-IF
      *
      *    剤内に手技料は１つのみ
           IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
               IF      IDX-SYU             >   ZERO
                   MOVE    "0019"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   GO      TO      100114-SYUGISYORI-CHK-EXT
               ELSE
                   MOVE    IDX-Z           TO  IDX-SYU
               END-IF
           END-IF
      *
      *    加算チェック
           IF      WRK-COM-DATAKBN (IDX-Z) =   "2"
               PERFORM 1005-KASAN-CHK-SEC
           END-IF
      *
      *    時間加算チェック
           IF     WRK-COM-TIMEFLG (IDX-Z)  NOT =   ZERO
               PERFORM 1002-KASAN-SET-SEC
           ELSE
      *        入院の場合、時間外区分も対象とする
               IF      (SPA-NYUGAIKBN      =   "1"  )  AND
                       (LNK-ORCSC-TIMEFLG  NOT =   ZERO)  AND
                       (WRK-COM-DATAKBN(IDX-Z) =   "1" )
               AND     (WRK-COM-TIMEKSNKBN(IDX-Z)   NOT =  ZERO )
                   PERFORM 1002-KASAN-SET-SEC
               END-IF
           END-IF
      *
      *    時間加算を入力された時
           IF     (WRK-COM-DATAKBN (IDX-Z)     =   "2"  )  AND
                  (WRK-COM-TIMEKSNKBN(IDX-Z)   =   1  OR
                                                   2  OR
                                                   3  OR
                                                   4  OR
                                                   5     )
               IF     (LNK-ORCSC-NENREI-YY     <   6  )
                   MOVE    1                   TO  FLG-TIME-NYU
               END-IF
           END-IF
      *
      *    包括まとめ入力は以下対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               OR (WRK-COM-HKNCOMBI (IDX-Z) =   "9999" )
               GO      TO      100114-SYUGISYORI-CHK-EXT
           END-IF
      *
      *----(01.00.03) LINE ADD START ----------------------------------
      *    診療情報提供料を入力時、算定日のコメントを自動加算
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "180016110"
                                               OR  "113009510"
               IF      LNK-ORCSC-PG        =   "K02"
                   PERFORM 10010-COMMT-SEC
               END-IF
           END-IF
      *
      *    特定薬剤治療加算料チェック
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "113000410"  OR
                                                   "180000110"  OR
                                                   "180000210"  OR
                                                   "113000510"  OR
                                                   "113000770"  OR
                                                   "113000670"
               PERFORM 10011-TOKUYAKU-SEC
           END-IF
      *
      *    悪性腫瘍特異物資治療管理料チェック
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "113001210"  OR
                                                   "113001310"  OR
                                                   "113002110"
               PERFORM 10012-AKUSEI-SEC
           END-IF
      *!!
      *    腫瘍マーカー検査初回月加算チェック
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "113001470" 
               PERFORM 10012-AKUSEI-SYOKAI-SEC
           END-IF
      *
      *    ニコチン依存管理料チェック
      *??  IF      WRK-COM-INPUTCD (IDX-Z)     =   "113008310"  OR
      *                                            "113008410"  OR
      *                                            "113008510"
      *        PERFORM 10014-NIKOCHIN-CHK-SEC
      *??  END-IF
      *
      *    小児特定疾患カウンセリング料チェック
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "113000810"
                                               OR  "113009910"
               IF      LNK-ORCSC-PG        =   "K02"
                   PERFORM 10015-SYOUNICA-CHK-SEC
               END-IF
           END-IF
      *
      *    アフターケアの時算定できないコードのチェック
           IF      SPA-RSI-HKNKBN      =   "3"
               PERFORM 100113-RSI-AFTCHK-SEC
               IF      WRK-ERRCD       NOT =   SPACE
                   GO      TO      100114-SYUGISYORI-CHK-EXT
               END-IF
           END-IF
      *
      *    ２００床以上で再診がある時
      *    外来診療料算定有無
           IF     (SPA-BEDSUIPN        >=  200 )  AND
                  (SPA-HOSPSBT         =   1   )  AND
                  (WRK-COM-SRYKBN (IDX-Z)  =   "12")
               MOVE    1                   TO  LNK-ORCSC-GAIRAISIN
           ELSE
               MOVE    ZERO                TO  LNK-ORCSC-GAIRAISIN
           END-IF
           .
       100114-SYUGISYORI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診察料の決定処理
      *****************************************************************
       100121-SINSATU-CD-SEC           SECTION.
      *
      *    診察料セット
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYCD
                   WHEN   (LNK-ORCSC-ROUJIN    =   TBL-ROUJIN(TBL-IDX))
                     AND  (SPA-HOSPSBT         =   TBL-HOSPSBT(TBL-IDX))
                     AND  (WRK-KBN             =   TBL-KBN    (TBL-IDX))
                       MOVE    TBL-SRYCD (TBL-IDX)
                                               TO  WRK-SRYCD
           END-SEARCH
      *
           .
       100121-SINSATU-CD-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算,継続管理加算の判定処理
      *****************************************************************
       100111-KEIZOKU-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-GAIRAI
           MOVE    ZERO                TO  FLG-KEIZOK
           MOVE    SPACE               TO  WRK-KBN
      *    外来管理加算
           IF      WRK-COM-INPUTCD(IDX-Z)  =   TBLG-SRYCD
               MOVE    "G"                 TO  WRK-KBN
               MOVE    1                   TO  FLG-GAIRAI
           END-IF
      **** SET     TBLG-IDX            TO  1
      *    SEARCH  TBL-GAIRAI-OCC      VARYING   TBLG-IDX
      *            AT   END
      *                MOVE    SPACE           TO  WRK-KBN
      *            WHEN    WRK-COM-INPUTCD(IDX-Z)  =   TBLG-SRYCD
      *                                                      (TBLG-IDX)
      *                MOVE    TBLG-KBN   (TBLG-IDX)
      *                                        TO  WRK-KBN
      *    END-SEARCH
      *    EVALUATE    WRK-KBN
      *        外来管理加算
      *        WHEN    "G"
      *            MOVE    1           TO  FLG-GAIRAI
      *        継続管理加算
      *        WHEN    "K"
      *            MOVE    1           TO  FLG-KEIZOK
      *    END-EVALUATE
      *
      *    健保で、継続管理の時
      **** IF     (SPA-HKNKBN      NOT =   1   ) OR
      *           (FLG-KEIZOK          =   1   )
      *        GO      TO      100111-KEIZOKU-CHK-EXT
      **** END-IF
      *
      *    ２００床以上の場合、エラー
           IF     (SPA-BEDSUIPN        >=  200 )  AND
                  (SPA-HOSPSBT         =   1   )
               IF     (FLG-KEIZOK          =   1 )  OR
                      (FLG-GAIRAI          =   1 )
                   MOVE    "8051"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   GO      TO      100111-KEIZOKU-CHK-EXT
               END-IF
           END-IF
      *
      *    健保の時
           IF     (SPA-HKNKBN          =   ZERO)
      *      自賠責の健保準拠以外
      *D     OR   (SPA-RSI-HKNKBN      =   "5" )
             OR   (SPA-RSI-JUNKYO      =   "1" )
      *      公害保険
             OR   (SPA-HKNKBN          =   2   )
               GO      TO      100111-KEIZOKU-CHK-EXT
           END-IF
      **** 労災で継続管理の時
      *    IF      FLG-KEIZOK          =   1
      *        IF      SPA-RSI-HKNKBN      =   "3"
      *            アフターケアの時エラー
      *            MOVE    "8006"          TO  WRK-ERRCD
      *            MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
      *        END-IF
      *        GO      TO      100111-KEIZOKU-CHK-EXT
      **** END-IF
      *
      *    労災保険の時、外来管理加算はエラーとする
           IF      WRK-KBN         =   "G"
               MOVE    "8001"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
               GO      TO      100111-KEIZOKU-CHK-EXT
           END-IF
      *
      *    労災保険の外来管理加算の判定
           MOVE    ZERO                TO  FLG-GAIRAI
           SET     TBL-RSI             TO  1
           SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
              AT   END
                   MOVE    ZERO                TO  FLG-ARI
                   MOVE    SPACE               TO  WRK-KBN
               WHEN    WRK-COM-INPUTCD(IDX-Z)  =   TBLR-SRYCD (TBL-RSI)
                   MOVE    1                   TO  FLG-ARI
                   MOVE    TBLR-KBN(TBL-RSI)   TO  WRK-KBN
           END-SEARCH
      *    労災の外来管理加算点数セット
           IF      WRK-KBN             =   "G"
               IF      SPA-RSI-HKNKBN      =   "3"
      *            アフターケアの時エラー
                   MOVE    "8007"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
               ELSE
                   MOVE    1               TO  FLG-GAIRAI
                   MOVE    WRK-COM-KISOTEN(IDX-Z)
                                           TO  LNK-ORCSC-GAIRAI-TEN
               END-IF
           END-IF
      *
           .
       100111-KEIZOKU-CHK-EXT.
           EXIT.
      *        
      *****************************************************************
      *    自賠責の健保準拠の診察料の判定チェック処理
      *****************************************************************
       10013-JBI-SINSATU-CHK-SEC       SECTION.
      *
           EVALUATE    WRK-COM-INPUTCD(IDX-Z)
      *       初診料・同日初診
               WHEN    "111000110"
               WHEN    "111011810"
      *            受診履歴判定
                   PERFORM 100131-RSIJYURRKCHK-SEC
           END-EVALUATE
           .
       10013-JBI-SINSATU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診算定時の受診チェック処理
      *****************************************************************
       100131-RSIJYURRKCHK-SEC       SECTION.
      *
      *    初診料の時、過去に受診がないこと
      *    受診歴を検索する  医療機関、患者ＩＤ
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-HKNCOMBINUM     TO  JYURRK-HKNCOMBINUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key15"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   ZERO
               IF      JYURRK-SRYYMD       <   SPA-SRYYMD
                   MOVE    "8002"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR
                                                         (IDX-Z)
               END-IF
           END-IF
           .
       100131-RSIJYURRKCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の診察料の判定チェック処理
      *****************************************************************
       10013-RSI-SINSATU-CHK-SEC       SECTION.
      *
           EVALUATE    WRK-COM-INPUTCD(IDX-Z)
      *    初診料・同日初診
           WHEN    "101110010"
           WHEN    "101110040"
      *        受診履歴判定
               PERFORM 100131-RSIJYURRKCHK-SEC
      *
      *    再診時療養指導管理料
           WHEN    "101120030"
      *        初診料算定時、エラー
               IF      LNK-ORCSC-SYOSIN2       =   1
                   MOVE    "8013"          TO  WRK-ERRCD
                    MOVE    "1"             TO  WRK-COM-CUR
                                                         (IDX-Z)
               END-IF
      *    救急医療管理加算（入院）入院日から７日以内
           WHEN    "101110020"
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    LNK-ORCSC-NYUINYMD  TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    7                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  WRK-NYUINYMD-7
               IF     (SPA-SRYYMD          >=  LNK-ORCSC-NYUINYMD)
                 AND  (SPA-SRYYMD          <   WRK-NYUINYMD-7 )
                   CONTINUE
               ELSE
                   MOVE    "8028"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR
                                                         (IDX-Z)
               END-IF
           END-EVALUATE
      *
      *    労災の時、労災コードで判定する
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-KAISU           =   ZERO
                   PERFORM 20012-RSISINSATU-CHK-SEC
               END-IF
           END-IF
      *
           .
       10013-RSI-SINSATU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    アフターケアの時算定できないコードのチェック処理
      *****************************************************************
       100113-RSI-AFTCHK-SEC           SECTION.
      *
      *    在宅自己導尿指導管理料チェック
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "114004410"
               MOVE    "8008"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
      *    てんかん指導料チェック
           IF      WRK-COM-INPUTCD (IDX-Z)     =   "113002850"
               IF      WRK-ERRCD       =   SPACE
                   MOVE    "8009"          TO  WRK-ERRCD
               END-IF
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *    再診時療養指導管理料
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "101120030"
               IF      WRK-ERRCD       =   SPACE
                   MOVE    "8012"          TO  WRK-ERRCD
               END-IF
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
      *    救急医療管理加算
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "101110020"  OR
                                               "101110030"
               IF      WRK-ERRCD       =   SPACE
                   MOVE    "8015"          TO  WRK-ERRCD
               END-IF
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *    労災の指導料加算等
           IF      WRK-COM-INPUTCD(IDX-Z)  >=  "101130010"  AND
                                           <=  "101130100"
               IF      WRK-ERRCD       =   SPACE
                   MOVE    "8016"          TO  WRK-ERRCD
               END-IF
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *    在宅料
      **** IF      WRK-COM-SRYKBN (IDX-Z)  =   "14"
      *        IF      WRK-ERRCD       =   SPACE
      *            MOVE    "8017"          TO  WRK-ERRCD
      *        END-IF
      *        MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
      **** END-IF
      *
           .
       100113-RSI-AFTCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算料チェック処理
      *****************************************************************
       1005-KASAN-CHK-SEC                  SECTION.
      *
      *    加算データの時、前に手技料があること
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >=  IDX-Z
               IF     (WRK-COM-INPUTCD(IDX1)(1:1)  =   "1" )  AND
                      (WRK-COM-DATAKBN(IDX1)   NOT =   "2" )
                   IF      WRK-COM-CHUKSNCD (IDX1)     =
                                               WRK-COM-CHUKSNCD(IDX-Z)
                       MOVE    1               TO  FLG-ARIOK
                   END-IF
                   MOVE    1               TO  FLG-ARI
                   MOVE    IDX-Z           TO  IDX1
               END-IF
      *
           END-PERFORM
      *
      *    当日小児科外来診察料を算定済みの時、時間外のみはＯＫとする
           IF      LNK-ORCSC-SYONIKA-ARI2  =   1
               PERFORM 10052-SYONIKASRYCD-CHK-SEC
      *        再診の時間外の時、ＯＫとする
               IF      WRK-SYONIKA-KBN     =   "4"
                   GO      TO      1005-KASAN-CHK-EXT
               END-IF
           END-IF
      *    寝たきり在宅があり、在宅指導料はＯＫとする
           IF     (LNK-ORCSC-NETAKIRI-ARI  NOT =   ZERO  )  OR
                  (LNK-ORCSC-NETAKIRI-ARI2 NOT =   ZERO  )
               IF      WRK-COM-SRYSYUKBN (IDX-Z)   =   "143"
                   MOVE    1                   TO  FLG-ARI
               END-IF
           END-IF
      *
      *    在宅加算料
           IF      WRK-COM-SRYSYUKBN (IDX-Z)  =   "143"
      *        在宅で、往診区分がないものは単独ＯＫとする
               IF     (WRK-COM-TNSSRYSYUKBN (IDX-Z)  =   "140" ) AND
                      (WRK-COM-OSINKBN (IDX-Z)       =   ZERO  )
                   MOVE    1                    TO  FLG-ARI
               END-IF
      *        １行目を手技とする
               IF     (FLG-ARI             =   1    )  AND
                      (IDX-SYU             =   ZERO )
                   MOVE    IDX-Z           TO  IDX-SYU
               END-IF
           END-IF
      *
           IF      FLG-ARI             =   ZERO
               MOVE    "0013"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
           .
       1005-KASAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    小児科外来診察料科チェックク処理
      *****************************************************************
       10052-SYONIKASRYCD-CHK-SEC                SECTION.
      *
      *    診察料検索
           SET     TBL-SYO             TO  1
           SEARCH  TBL-SYONIKA-OCC     VARYING   TBL-SYO
               AT  END
                   MOVE    SPACE           TO  WRK-SYONIKA-KBN
                WHEN   WRK-COM-INPUTCD (IDX-Z)
                                           =   TBL-SYONIKA-SRYCD
                                                      (TBL-SYO)
                   MOVE    TBL-SYONIKA-KBN (TBL-SYO)
                                           TO  WRK-SYONIKA-KBN
           END-SEARCH
           .
       10052-SYONIKASRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間加算追加処理
      *****************************************************************
       1002-KASAN-SET-SEC              SECTION.
      *
      *    入院の場合、時間外区分も対象とする
           IF      (SPA-NYUGAIKBN      =   "1"     )  AND
                   (LNK-ORCSC-TIMEFLG  NOT =   ZERO)  AND
                   (WRK-COM-TIMEFLG(IDX-Z) =   ZERO)
               MOVE    LNK-ORCSC-TIMEFLG       TO  WRK-COM-TIMEFLG
                                                              (IDX-Z)
           END-IF
           IF     (SPA-NYUGAIKBN       =   "1"  )  AND
                  (WRK-COM-TIMEFLG(IDX-Z)  NOT =   ZERO )
      *        時間外区分の無い場合はクリアする
               IF     (WRK-GMN-INPUTCD(IDX-Z)(1:1)     NUMERIC) AND
                      (WRK-GMN-INPUTCD(IDX-Z)(2:1)     =   SPACE )  AND
                      (WRK-GMN-INPUTCD(IDX-Z)(3:)  NOT =   SPACE )
                   CONTINUE
               ELSE
                   IF      LNK-ORCSC-TIMEFLG       =   ZERO
                       MOVE    ZERO                TO  WRK-COM-TIMEFLG
                                                                (IDX-Z)
                   ELSE
                       MOVE    LNK-ORCSC-TIMEFLG   TO  WRK-COM-TIMEFLG
                                                              (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *!!TEST
      *    画面に時間外区分がない時、追加する
           IF      (SPA-NYUGAIKBN      =   "2"  )  AND
                   (WRK-COM-DATAKBN (IDX-Z) =   "1" )
               IF     (WRK-GMN-INPUTCD (IDX-Z)(1:1)
                                           =   WRK-COM-TIMEFLG(IDX-Z))
                 AND  (WRK-GMN-INPUTCD (IDX-Z)(2:1) =   SPACE )
                 AND  (WRK-GMN-INPUTCD (IDX-Z)(3:)  NOT =   SPACE)
                   CONTINUE
               ELSE
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)  TO  WRK-INPUTCD
                   MOVE    SPACE           TO  WRK-GMN-INPUTCD(IDX-Z)
                   MOVE    WRK-COM-TIMEFLG(IDX-Z)
                                           TO  WRK-GMN-INPUTCD(IDX-Z)
                                                              (1:1)
                   MOVE    WRK-INPUTCD     TO  WRK-GMN-INPUTCD(IDX-Z)
                                                              (3:1)
               END-IF
           END-IF
      *!!TEST
      *    診察の時、他の時間加算もする（外来のみ）
           IF       SPA-NYUGAIKBN      =   "2"
               MOVE    WRK-COM-TIMEFLG(IDX-Z)
                                       TO  LNK-ORCSC-TIMEFLG
               IF      WRK-COM-TIMEFLG(IDX-Z)  =   5
                                               OR  6
                                               OR  7
      *           早朝加算は診察料のみ
                                               OR  8
                  MOVE    ZERO             TO  LNK-ORCSC-TIMEFLG
               END-IF
           END-IF
      *
      *    夜間・早朝加算
           IF      WRK-COM-TIMEFLG(IDX-Z)  =   8
             AND  (LNK-ORCSC-PG        =   "K02" )
               IF     (SPA-YAKANKBN        =   1  )
                   CONTINUE
               ELSE
                   MOVE    "0210"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   GO      TO      1002-KASAN-SET-EXT
               END-IF
           END-IF
      *
      *    ６歳以上の乳幼児特例時間外エラー
           IF      WRK-COM-TIMEFLG(IDX-Z)  =   5
                                           OR  6
                                           OR  7
               IF     (LNK-ORCSC-NENREI-YY     <   6  )  AND
      *            時間外加算特例乳幼児加算算定ありの場合のみ
                      (LNK-ORCSC-JIKANTOKU-NYU =   1  )
                   CONTINUE
               ELSE
                   MOVE    "0112"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   GO      TO      1002-KASAN-SET-EXT
               END-IF
           END-IF
      *    同日初診
           IF     (WRK-COM-SRYKBN(IDX-Z)   =   "11"  )  AND
                  (WRK-KBN2                =   "D"   )
               GO      TO      1002-KASAN-SET-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-SRYCD
           MOVE    ZERO                TO  FLG-SINSATU
      *
      *    時間加算
           EVALUATE    WRK-COM-SRYKBN(IDX-Z)
              WHEN     "11"
                  MOVE    1                   TO  FLG-SINSATU
      *           初診料加算
                  EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
      *            /* 時間外 */
                       WHEN    1
                           MOVE    "111000570"     TO  WRK-SRYCD
      *            /* 休日 */
                       WHEN    2
                           MOVE    "111000670"     TO  WRK-SRYCD
      *            /* 深夜 */
                       WHEN    3 
                           MOVE    "111000770"     TO  WRK-SRYCD
      *            /* 時間外特例 */
                       WHEN    4
                           MOVE    "111000870"     TO  WRK-SRYCD
      *            早朝加算
                       WHEN    8
                           MOVE    "111012470"     TO  WRK-SRYCD
                  END-EVALUATE
              WHEN     "12"
      *           再診料加算
                  IF      WRK-KAISU                =   3
      *               今回が外来診察料である時
                      PERFORM 10022-GAIRAISIN-SET-SEC
                      MOVE    3                   TO  FLG-SINSATU
                  ELSE
      *
                      MOVE    2                   TO  FLG-SINSATU
      *
                  EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
      *            /* 時間外 */
                       WHEN    1
                           MOVE    "112001110"     TO  WRK-SRYCD
      *            /* 休日 */
                       WHEN    2
                           MOVE    "112001210"     TO  WRK-SRYCD
      *            /* 深夜 */
                       WHEN    3
                           MOVE    "112001310"     TO  WRK-SRYCD
      *            /* 時間外特例 */
                       WHEN    4
                           MOVE    "112001410"     TO  WRK-SRYCD
      *            早朝加算
                       WHEN    8
                           MOVE    "112015570"     TO  WRK-SRYCD
                   END-EVALUATE
      *
               END-IF
           END-EVALUATE
      *
      *    小児科外来診療科の加算
           IF      WRK-COM-TIMEFLG(IDX-Z)  NOT =   ZERO
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *            初診
                   WHEN    "113003510"
                   WHEN    "113003710"
                       MOVE    4                   TO  FLG-SINSATU
                       EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
                           WHEN    1
      *                        初診時時間外加算
                               MOVE    "113009670"     TO  WRK-SRYCD
                           WHEN    2
      *                        初診時休日加算
                               MOVE    "113007170"     TO  WRK-SRYCD
                           WHEN    3
      *                        初診時深夜加算
                               MOVE    "113007270"     TO  WRK-SRYCD
                           WHEN    4
      *                        初診時  時間外特例
                               MOVE    "111010770"     TO  WRK-SRYCD
                       END-EVALUATE
      *            再診
                   WHEN    "113003610"
                   WHEN    "113003810"
      *                病院で一般病床数200以上場合
                       IF     (SPA-BEDSUIPN        >=  200 )  AND
                              (SPA-HOSPSBT         =   1   )
                           PERFORM 10023-SYONIKA-GAIRAI-SEC
                       ELSE
      *
                       MOVE    5                   TO  FLG-SINSATU
                       EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
                           WHEN    1
      *                        再診時時間外加算
                               MOVE    "113009770"     TO  WRK-SRYCD
                           WHEN    2
      *                        再診時休日加算
                               MOVE    "113007470"     TO  WRK-SRYCD
                           WHEN    3
      *                        再診時深夜加算
                               MOVE    "113007570"     TO  WRK-SRYCD
                           WHEN    4
      *                        再診時  時間外特例
                               MOVE    "112006070"     TO  WRK-SRYCD
                       END-EVALUATE
      *
                       END-IF
      *
               END-EVALUATE
           END-IF
      *
      *    ６歳未満の時間外加算
           IF     (LNK-ORCSC-NENREI-YY     <   6  )
               PERFORM 1009-NYUYOUJI-KSN-SEC
           END-IF
      *
           IF      WRK-SRYCD            =    SPACE
               GO      TO      1002-KASAN-SET-EXT
           END-IF
      *
      *    時間自動加算　存在チェック
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     ) OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF     (WRK-COM-DATAKBN    (IDX1)       =   "2"  )  AND
                      (WRK-COM-TIMEKSNKBN (IDX1)   NOT =   ZERO )
                    MOVE    1              TO  FLG-ARI
                    MOVE    IDX1           TO  IDX-T
               END-IF
           END-PERFORM
      *    自動追加
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
      *
      *????
      *    深夜・早朝加算の施設規準
      *    IF      FLG-ADD             =   ZERO
      *        IF     (WRK-COM-TIMEFLG(IDX-Z)  =   8  )
      *          AND  (FLG-KIJUN-ERR        =  1      )
      *            MOVE    "0112"          TO  WRK-ERRCD
      *            MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
      *        END-IF
      **???END-IF
      *
           .
       1002-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来診察料時間外編集処理
      *****************************************************************
       10022-GAIRAISIN-SET-SEC        SECTION.
      *
           EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
      *            /* 時間外 */
               WHEN    1
                   MOVE    "112006470"         TO  WRK-SRYCD
      *            /* 休日 */
               WHEN    2
                   MOVE    "112006570"         TO  WRK-SRYCD
      *            /* 深夜 */
               WHEN    3
                   MOVE    "112006670"         TO  WRK-SRYCD
      *            /* 時間外特例 */
               WHEN    4
                   MOVE    "112006770"         TO  WRK-SRYCD
           END-EVALUATE
      *
           .
       10022-GAIRAISIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来診察料時間外編集処理
      *****************************************************************
       10023-SYONIKA-GAIRAI-SEC        SECTION.
      *
           MOVE    6                   TO  FLG-SINSATU
           EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
               WHEN    1
      *            外来診察料時間外加算
                   MOVE    "113009870"     TO  WRK-SRYCD
               WHEN    2
      *            外来診察料休日加算
                   MOVE    "113007770"     TO  WRK-SRYCD
               WHEN    3
      *            外来診察料深夜加算
                   MOVE    "113007870"     TO  WRK-SRYCD
               WHEN    4
      *            外来診察料特定加算
                   MOVE    "113005570"     TO  WRK-SRYCD
           END-EVALUATE
      *
           .
       10023-SYONIKA-GAIRAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算　存在チェック加処理
      *****************************************************************
       1110-KASAN-CHK-SEC        SECTION.
      *
      *    時間自動加算　存在チェック
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   ) OR
                              (FLG-ARI                 =   1  ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
             IF    WRK-COM-HKNCOMBI (IDX1) NOT =   "9999"
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                    MOVE    IDX1           TO  IDX-T
                    MOVE    1              TO  FLG-ARI
               END-IF
             END-IF
           END-PERFORM
           .
       1110-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算１行追加処理
      *****************************************************************
       1100-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           IF      FLG-ARI             =   ZERO
               ADD     1               TO  IDX-S
           ELSE
               MOVE    IDX-T           TO  IDX-S
           END-IF
      *
      *    点数マスタ編集・基本チェック
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
           INITIALIZE                  ORCSC92AREA
           IF      FLG-ARI             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-S)
               MOVE    SPACE               TO  WRK-COM-GMNFLG (IDX-S)
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-15MIMAN-ARI   TO  LNK-SC92-15MIMAN-ARI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    小児科外来診療科の時、年齢エラーはそのまま
           IF      LNK-ORCSC-SYONIKA-ARI   NOT =   ZERO
               IF      LNK-SC92-ERRCD5 NOT =   SPACE
                   MOVE    SPACE           TO  LNK-SC92-ERRCD5
               END-IF
           END-IF
      *
      *    エラーの時、追加なし
           IF      LNK-SC92-ERRCD4     =   "0024"
               MOVE    1               TO  FLG-KIJUN-ERR
           ELSE
               MOVE    0               TO  FLG-KIJUN-ERR
           END-IF
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
               MOVE    SPACE               TO  SPA-ERRCD
               IF      FLG-ARI             =   ZERO
                   COMPUTE IDX-S           =   IDX-S
                                           -   1
               END-IF
               GO      TO      1100-KASAN-SET-EXT
           END-IF
      *
           IF      IDX-S               >   1
               MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                           TO  WRK-COM-SRYKBN(IDX-S)
               MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
               MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                           TO  WRK-COM-SRYSYUKBN(IDX-S)
           END-IF
      *
      *    自動追加分（入力時に追加）
           MOVE    "2"                 TO  WRK-COM-AUTOKBN (IDX-S)
      *
           MOVE    1                   TO  FLG-ADD
      *
           .
       1100-KASAN-SET-EXT.
           EXIT.
      *****************************************************************
      *    年齢加算追加処理
      *****************************************************************
       1007-NENREI-SYORI-SEC           SECTION.
      *
           MOVE    IDX-SYU             TO  IDX-Z
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-Z IDX2)
                                          =   SPACE  OR  ZERO )
               PERFORM 10071-NENREI-CHK-SEC
               IF      FLG-NENREI-OK          =   1
      *
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-Z IDX2)  TO  WRK-SRYCD
      *        加算チェック
               PERFORM 1110-KASAN-CHK-SEC
               MOVE    IDX-Z               TO  IDX-S
      *        加算追加
               PERFORM 1100-KASAN-SET-SEC
               IF      FLG-ADD             =   1
                   MOVE    4                   TO  IDX2
                   MOVE    WRK-SRYCD           TO  WRK-NYU-SRYCD
               END-IF
      *
               END-IF
           END-PERFORM
      *
           .
       1007-NYUYOJI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算時間外自動振替処理
      *****************************************************************
       10071-NENREI-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-NENREI-OK
      *
           MOVE    ZERO                TO  WRK-KGNAGE
           MOVE    ZERO                TO  WRK-JGNAGE
           MOVE    WRK-COM-AGEKSNKGN  (IDX-Z IDX2)
                                       TO  WRK-KGNAGE(2:2)
           MOVE    WRK-COM-AGEKSNJGN  (IDX-Z IDX2)
                                       TO  WRK-JGNAGE(2:2)
      *
      *    下限年齢チェック
           IF      WRK-COM-AGEKSNKGN  (IDX-Z IDX2) =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      WRK-COM-AGEKSNKGN  (IDX-Z IDX2) =   "AA"
      *            新生児判定
                   IF     (LNK-ORCSC-NENREI-YY     =   ZERO )  AND
                          (LNK-ORCSC-NENREI-MM     =   ZERO )  AND
                          (LNK-ORCSC-NENREI-DD     <   28   )
                       MOVE    ZERO                TO  FLG-NENREI-OK
                   ELSE
                       MOVE    1                   TO  FLG-NENREI-OK
                   END-IF
               ELSE
                   IF      LNK-ORCSC-NENREI-YY     >=  WRK-KGNAGE
                       MOVE    1                   TO  FLG-NENREI-OK
                   ELSE
                       MOVE    ZERO                TO  FLG-NENREI-OK
                   END-IF
               END-IF
           END-IF
      *
      *    上限年齢チェック
           IF      WRK-COM-AGEKSNJGN  (IDX-Z IDX2) =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      WRK-COM-AGEKSNJGN  (IDX-Z IDX2) =   "AA"
      *            新生児判定
                   IF     (LNK-ORCSC-NENREI-YY     =   ZERO )  AND
                          (LNK-ORCSC-NENREI-MM     =   ZERO )  AND
                          (LNK-ORCSC-NENREI-DD     <   28   )
                       MOVE    1                   TO  FLG-NENREI-OK
                   ELSE
                       MOVE    ZERO                TO  FLG-NENREI-OK
                   END-IF
               ELSE
                   IF      LNK-ORCSC-NENREI-YY     <   WRK-JGNAGE
                       MOVE    1                   TO  FLG-NENREI-OK
                   ELSE
                       MOVE    ZERO                TO  FLG-NENREI-OK
                   END-IF
               END-IF
           END-IF
      *
           .
       10071-NENREI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算時間外自動振替処理
      *****************************************************************
      *1008-NENREI-CHENGE-SEC           SECTION.
      *
      *    MOVE    WRK-NYU-SRYCD       TO  WRK-SRYCD
      *    加算存在チェック
      *    PERFORM 1110-KASAN-CHK-SEC
      *    IF      FLG-ARI             =   ZERO
      *        GO      TO      1008-NENREI-CHENGE-EXT
      *    END-IF
      *
      *    MOVE    IDX-SYU             TO  IDX-S
      *    EVALUATE    WRK-COM-SRYKBN(IDX-SYU)
      *        WHEN    "11"
      *            初診
      *            MOVE    "111011470"         TO  WRK-SRYCD
      *            PERFORM 1100-KASAN-SET-SEC
      *        WHEN    "12"
      *            IF      WRK-KAISU                =   3
      *                外来診察料（乳幼児）（時間加算等）
      *                MOVE    "112007270"         TO  WRK-SRYCD
      *                PERFORM 1100-KASAN-SET-SEC
      *DDDDDDDDDD      IF      FLG-ADD             =   ZERO
      *                    外来診察料（幼児）（時間加算等）
      *                    MOVE    "112007370"         TO  WRK-SRYCD
      *                    PERFORM 1100-KASAN-SET-SEC
      *DDDDDDDDD       END-IF
      *            ELSE
      *                再診（乳幼児）（時間加算等）
      *                MOVE    "112006970"         TO  WRK-SRYCD
      *                PERFORM 1100-KASAN-SET-SEC
      *DDDDDDDDDDD     IF      FLG-ADD             =   ZERO
      *                    再診（幼児）（時間加算等）
      *                    MOVE    "112007070"         TO  WRK-SRYCD
      *                    PERFORM 1100-KASAN-SET-SEC
      *DDDDDDDDDDDDDDD END-IF
      *            END-IF
      *    END-EVALUATE
      ****
      *    .
      *1008-NENREI-CHENGE-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    ６歳未満の乳幼児加算追加処理
      *****************************************************************
       1009-NYUYOUJI-KSN-SEC           SECTION.
      *
           EVALUATE    FLG-SINSATU
      *        初診
               WHEN    1
                   EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
                       WHEN    1
      *                    時間外
                           MOVE    "111011970"     TO  WRK-SRYCD
                       WHEN    2
      *                    休日
                           MOVE    "111012070"     TO  WRK-SRYCD
                       WHEN    3
      *                    深夜
                           MOVE    "111012170"     TO  WRK-SRYCD
                       WHEN    4
      *                    時間外特例
                           MOVE    "111012270"     TO  WRK-SRYCD
                       WHEN    5
      *                    夜間（時間外）加算
                           MOVE    "111011570"     TO  WRK-SRYCD
                       WHEN    6
      *                    休日加算
                           MOVE    "111011670"     TO  WRK-SRYCD
                       WHEN    7
      *                    深夜加算
                           MOVE    "111011770"     TO  WRK-SRYCD
                   END-EVALUATE
      *        再診
               WHEN    2
                   EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
                       WHEN    1
      *                    時間外
                           MOVE    "112014770"     TO  WRK-SRYCD
                       WHEN    2
      *                    休日
                           MOVE    "112014870"     TO  WRK-SRYCD
                       WHEN    3
      *                    深夜
                           MOVE    "112014970"     TO  WRK-SRYCD
                       WHEN    4
      *                    時間外特例
                           MOVE    "112015070"     TO  WRK-SRYCD
                       WHEN    5
      *                    夜間（時間外）加算
                           MOVE    "112014170"     TO  WRK-SRYCD
                       WHEN    6
      *                    休日加算
                           MOVE    "112014270"     TO  WRK-SRYCD
                       WHEN    7
      *                    深夜加算
                           MOVE    "112014370"     TO  WRK-SRYCD
                   END-EVALUATE
      *        外来診察料
               WHEN    3
                   EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
                       WHEN    1
      *                    時間外
                           MOVE    "112015170"     TO  WRK-SRYCD
                       WHEN    2
      *                    休日
                           MOVE    "112015270"     TO  WRK-SRYCD
                       WHEN    3
      *                    深夜
                           MOVE    "112015370"     TO  WRK-SRYCD
                       WHEN    4
      *                    時間外特例
                           MOVE    "112015470"     TO  WRK-SRYCD
                       WHEN    5
      *                    夜間（時間外）加算
                           MOVE    "112014470"     TO  WRK-SRYCD
                       WHEN    6
      *                    休日加算
                           MOVE    "112014570"     TO  WRK-SRYCD
                       WHEN    7
      *                    深夜加算
                           MOVE    "112014670"     TO  WRK-SRYCD
                   END-EVALUATE
      *        小児科外来診察料（初診）
               WHEN    4
                   EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
                       WHEN    5
      *                    夜間（時間外）加算
                           MOVE    "113007070"     TO  WRK-SRYCD
                       WHEN    6
      *                    休日加算
                           MOVE    "113007170"     TO  WRK-SRYCD
                       WHEN    7
      *                    深夜加算
                           MOVE    "113007270"     TO  WRK-SRYCD
                   END-EVALUATE
      *        小児科外来診察料（再診）
               WHEN    5
                   EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
                       WHEN    5
      *                    夜間（時間外）加算
                           MOVE    "113007370"     TO  WRK-SRYCD
                       WHEN    6
      *                    休日加算
                           MOVE    "113007470"     TO  WRK-SRYCD
                       WHEN    7
      *                    深夜加算
                           MOVE    "113007570"     TO  WRK-SRYCD
                   END-EVALUATE
      *        小児科外来診察料（外来診察料）
               WHEN    6
                   EVALUATE    WRK-COM-TIMEFLG(IDX-Z)
                       WHEN    5
      *                    夜間（時間外）加算
                           MOVE    "113007670"     TO  WRK-SRYCD
                       WHEN    6
      *                    休日加算
                           MOVE    "113007770"     TO  WRK-SRYCD
                       WHEN    7
      *                    深夜加算
                           MOVE    "113007870"     TO  WRK-SRYCD
                   END-EVALUATE
      *
           END-EVALUATE
      *
           IF      WRK-SRYCD       NOT =   SPACE
               IF      WRK-COM-TIMEFLG(IDX-Z)  NOT =   "8"
                   MOVE    1               TO  FLG-TIME-NYU
               END-IF
           END-IF
           .
       1009-NYUYOUJI-KSN-EXT.
           EXIT.
      *****************************************************************
      *    診療情報提供算定日追加処理
      *****************************************************************
       10010-COMMT-SEC           SECTION.
      *    診療情報提供料算定日
           MOVE    "840000052"         TO  WRK-SRYCD
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      10010-COMMT-EXT
           END-IF
      *
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   ZERO
               GO      TO      10010-COMMT-EXT
           END-IF
      *    診療日を編集
           MOVE    SPA-SRYYMD(7:2)     TO  WRK-COM-ATAI1 (IDX-S)
      *
      *    画面再編集
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    LNK-ORCSC-PG        TO  LNK-SC94-PG
           MOVE    IDX-S               TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    診療日を編集
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-15MIMAN-ARI   TO  LNK-SC92-15MIMAN-ARI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
           .
       10010-COMMT-EXT.
           EXIT.
      *****************************************************************
      *    悪性腫瘍特異物資治療管理料のコメント加算処理
      *****************************************************************
       10012-AKUSEI-SEC           SECTION.
      *
      *    腫瘍マーカ検査をコメントへ
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   ) OR
                              (FLG-ARI                 =   1  ) OR
                             ((WRK-COM-INPUTCD (IDX1)  =   SPACE ) AND
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE ))
               IF      (WRK-COM-CDKBN-KBN (IDX1)   =   "D" )  AND
                       (WRK-COM-CDKBN-KBNNUM(IDX1) =   "009")
                   PERFORM 100121-AKUSEI-COMMENT-SEC
               END-IF
           END-PERFORM
      *
      *    腫瘍マーカー検査名
           MOVE    "830000015"         TO  WRK-SRYCD
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      10012-AKUSEI-EXT
           END-IF
      *@
           IF      WRK-COM-INPUTCD (IDX-Z + 1) =   "113001470" 
               COMPUTE IDX-Z2              =   IDX-Z   +  1
           ELSE
               MOVE    IDX-Z               TO  IDX-Z2
           END-IF
      *
      *    腫瘍マーカー一覧展開選択
           IF      LNK-ORCSC-AKUSEIHYOFLG  =   "1"
      *        診療行為一覧へ遷移
               MOVE    "AK98"              TO  WRK-ERRCD
               MOVE    "A"                 TO  WRK-COM-CUR (IDX-Z2)
           ELSE
      *        コメント自動発生
               MOVE    IDX-Z2               TO  IDX-S
               PERFORM 1100-KASAN-SET-SEC
               IF      FLG-ADD             =   1
                   MOVE    "1"                 TO  WRK-COM-CUR (IDX-S)
               END-IF
           END-IF
      *
           .
       10012-AKUSEI-EXT.
           EXIT.
      *****************************************************************
      *    腫瘍マーカー検査初回月加算チェック処理
      *****************************************************************
       10012-AKUSEI-SYOKAI-SEC           SECTION.
      *
           IF      WRK-ERRCD           =   SPACE
      *        悪性腫瘍特異物資治療管理料が算定済みはエラー
               PERFORM 100121-AKUSEI-SANTEI-SEC
               IF      FLG-SANTEI-ARI      =   1
                   IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE )
                      AND (LNK-ORCSC-KEI-IDX       =   ZERO  )
                       MOVE    IDX-Z            TO  LNK-ORCSC-KEI-IDX
                       MOVE    "K134"           TO  WRK-ERRCD
                       MOVE    "1"              TO  WRK-COM-CUR
                                                             (IDX-Z)
                       MOVE    "1"              TO  WRK-COM-KZMFLG
                                                              (IDX-Z)
                       MOVE    WRK-AKUSEI-YMD  TO  LNK-ORCSC-ERRMSG
                   END-IF
               END-IF
           END-IF
           .
       10012-AKUSEI-SYOKAI-EXT.
           EXIT.
      *@
      *****************************************************************
      *    悪性腫瘍特異物資治療管理料算定済チェック処理
      *****************************************************************
       100121-AKUSEI-SANTEI-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-SANTEI-ARI
           MOVE    SPACE               TO  WRK-AKU-YMD
      *
           MOVE    "113001210"         TO  WRK-SRYCD
           PERFORM 1001211-AKUSEI-SANTEIKEN-SEC
      *
           MOVE    "113001310"         TO  WRK-SRYCD
           PERFORM 1001211-AKUSEI-SANTEIKEN-SEC
      *
           MOVE    "113002110"         TO  WRK-SRYCD
           PERFORM 1001211-AKUSEI-SANTEIKEN-SEC
      *
           MOVE    SPACE               TO  WRK-AKUSEI-YMD
           IF      WRK-AKU-YMD     NOT =   SPACE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-AKU-YMD         TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  "前回算定日："
                       LNK-DAY2-EDTYMD3    DELIMITED  BY  SIZE
                      INTO                 WRK-AKUSEI-YMD
               END-STRING
           END-IF
           .
       100121-AKUSEI-SANTEI-EXT.
           EXIT.
      *@
      *****************************************************************
      *    特定薬剤治療加算料チェック処理
      *****************************************************************
       1001211-AKUSEI-SANTEIKEN-SEC    SECTION.
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    SPA-SRYYMD(1:6)     TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key15"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           PERFORM     UNTIL   FLG-SANTEI      =   1
               MOVE    ZERO                TO  WRK-LST-DD
               MOVE    SANTEI-SRYYM        TO  WRK-LST-YMD(1:6)
               PERFORM VARYING     IDX-Y2  FROM    1   BY  1
                       UNTIL       IDX-Y2      >   31
                   IF      SANTEI-DAY (IDX-Y2) >   ZERO
                       MOVE    IDX-Y2          TO  WRK-LST-DD
                       IF      WRK-LST-YMD     >=  SPA-SRYYMD
                           MOVE    31          TO  IDX-Y2
                       END-IF
                   END-IF
               END-PERFORM
      *        訂正の時、当日分はチェックしない
               IF     (LNK-ORCSC-SYORIFLG   =   1  )  AND
                      (WRK-LST-YMD          =   SPA-SRYYMD)
                   CONTINUE
               ELSE
                   MOVE    1                TO  FLG-SANTEI-ARI
                   MOVE    1                TO  FLG-SANTEI
                   IF      WRK-LST-YMD      >=  WRK-AKU-YMD
                       MOVE    WRK-LST-YMD      TO  WRK-AKU-YMD
                   END-IF
               END-IF
               IF      FLG-SANTEI          =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key15"             TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1001211-AKUSEI-SANTEIKEN-EXT.
           EXIT.
      *
      *@
      *****************************************************************
      *    特定薬剤治療加算料チェック処理
      *****************************************************************
       100121-AKUSEI-COMMENT-SEC              SECTION.
      *
           MOVE    WRK-COM-INPUTCD (IDX1)  TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-NAME            TO  WRK-NAME
           ELSE
               MOVE    WRK-COM-NAME (IDX1) TO  WRK-NAME
           END-IF
           MOVE    IDX1                TO  IDX-T
           MOVE    "830000015"         TO  WRK-SRYCD
      *    コメントコードへ
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
           INITIALIZE                  ORCSC92AREA
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-T)
           MOVE    SPACE               TO  WRK-COM-GMNFLG (IDX-T)
           MOVE    SPACE               TO  WRK-GMN-MEISYO-G (IDX-T)
           MOVE    IDX-T               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-15MIMAN-ARI   TO  LNK-SC92-15MIMAN-ARI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    HZN-SCR-REC        TO  WRK-SCR-REC
               MOVE    SPACE               TO  SPA-ERRCD
           ELSE
               MOVE    WRK-COM-SRYKBN (IDX-T - 1)
                                           TO  WRK-COM-SRYKBN(IDX-T)
               MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-T)
               MOVE    WRK-COM-SRYSYUKBN (IDX-T - 1)
                                           TO  WRK-COM-SRYSYUKBN(IDX-T)
      *        自動追加分（入力時に追加）
               MOVE    "2"                 TO  WRK-COM-AUTOKBN (IDX-T)
      *
               MOVE    WRK-GMN-MEISYO (IDX-T)
                                            TO  WRK-MEISYO-G
               MOVE    SPACE                TO  WRK-GMN-MEISYO (IDX-T)
               STRING  WRK-MEISYO-G         DELIMITED  BY  "  "
                       WRK-NAME             DELIMITED  BY  "  "
                           INTO                WRK-GMN-MEISYO
                                                  (IDX-T)
                END-STRING
                MOVE    WRK-GMN-INPUTCD (IDX-T)
                                           TO  WRK-MAE-INPUTCD (IDX-T)
                MOVE    SPACE              TO  WRK-COM-IN (IDX-T)
           END-IF
      *
           .
       100121-AKUSEI-COMMENT-EXT.
           EXIT.
      *@
      *****************************************************************
      *    特定薬剤治療加算料チェック処理
      *****************************************************************
       10011-TOKUYAKU-SEC              SECTION.
      *
      *    特定薬剤治療管理料算定なしの時、チェックしない
           IF      LNK-ORCSC-TOKUTEI-FLG   =   "1"
               CONTINUE
           ELSE
               GO      TO      10011-TOKUYAKU-EXT
           END-IF
      *
           MOVE    ZERO                 TO  IDX-S
      *    初回算定コメント判定
           PERFORM 100110-COMMNT-CHK-SEC
      *    初回算定日の編集
           MOVE    SPACE               TO  WRK-SYOKAI-YMD
           MOVE    ZERO                TO  WRK-SYOKAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
      *        算定履歴検索（診療日降順）
               SET     TBL-TOK             TO  IDX
               MOVE    TBL-TOKSRYCD  (TBL-TOK) TO  WRK-SRYCD
               INITIALIZE                  SANTEI-REC
               MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
               MOVE    SPA-PTID            TO  SANTEI-PTID
               MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
      *
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SANTEI
               END-IF
               PERFORM
                   UNTIL   FLG-SANTEI          =   1
                   IF     (SANTEI-MSANTEICNT   >   ZERO      ) AND
                          (SANTEI-FSANTEIYMD   <=  SPA-SRYYMD)
                       IF      SANTEI-FSANTEIYMD   >=  WRK-SYOKAI-YMD
                           MOVE    SANTEI-FSANTEIYMD
                                                  TO  WRK-SYOKAI-YMD
                       END-IF
      *                当月算定あり
                       IF      SANTEI-SRYYM        =   SPA-SRYYMD(1:6)
                           MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                           MOVE    SANTEI-MSANTEICNT   TO  WRK-SYOKAI
                           IF     (LNK-ORCSC-SYORIFLG  =   1  ) AND
                                  (SANTEI-DAY (IDX-Y2) >   ZERO)
                               COMPUTE WRK-SYOKAI      =   WRK-SYOKAI
                                                       -   SANTEI-DAY
                                                              (IDX-Y2)
                           END-IF
                       END-IF
                   END-IF
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-PERFORM
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
      *
      *    訂正の時、初回算定日と診療日が同じ時、初回分とする
           IF     (LNK-ORCSC-SYORIFLG   =   1  )  AND
                  (WRK-SYOKAI-YMD       =   SPA-SRYYMD)
      *        初回算定年月
               IF      (WRK-COM-YM      NOT =   SPACE )  AND
                       (WRK-COM-YM          <   WRK-SYOKAI-YMD(1:6))
                   CONTINUE
               ELSE
                   MOVE    SPACE            TO  WRK-SYOKAI-YMD
               END-IF
           END-IF
           IF     (LNK-ORCSC-SYORIFLG   =   1  )  AND
                  (WRK-SYOKAI-YMD       <   SPA-SRYYMD)
      *        初回算定年月
               IF      (WRK-COM-YM      NOT =   SPACE )  AND
                       (WRK-COM-YM          >   WRK-SYOKAI-YMD(1:6))
                 AND   (WRK-COM-YM          =   SPA-SRYYMD (1:6))
                   MOVE    SPACE            TO  WRK-SYOKAI-YMD
               END-IF
           END-IF
      *
      *    すでに初回算定日がある時、初回算定日に入れる
           IF      LNK-ORCSC-TOKTEI-YMD    NOT =   SPACE
               MOVE    LNK-ORCSC-TOKTEI-YMD    TO  WRK-SYOKAI-YMD
           END-IF
      *
      *    特定薬剤治療管理加算がある時、初回とする
           MOVE    ZERO                TO  FLG-SYOKAI3
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE      ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE ) OR
                              (WRK-ERRCD       NOT =   SPACE )
             IF    WRK-COM-HKNCOMBI (IDX1) NOT =   "9999"
               EVALUATE    WRK-COM-INPUTCD (IDX1)
                   WHEN    "113000770"
      *            特定薬剤治療管理料が当月存在しないこと
                       MOVE    "113000410"         TO  WRK-SRYCD
                       MOVE    "key"               TO  WRK-DBPATH
                       PERFORM 100111-SANTEI-KEN-SEC
                       IF     (FLG-SANTEI          =   1  )  OR
                              (LNK-ORCSC-SYORIFLG  =   1  AND
                               SPA-SRYYMD(7:2)     =   SANTEI-MSANTEID)
                           MOVE    SPACE           TO  WRK-SYOKAI-YMD
                           MOVE    1               TO  FLG-SYOKAI3
                       ELSE
      *!                   MOVE    "0132"      TO  WRK-ERRCD
      *!                   MOVE    "1"         TO  WRK-COM-CUR (IDX-Z)
                           IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE )
                              AND (LNK-ORCSC-KEI-IDX       =   ZERO  )
                               MOVE    IDX-Z            TO
                                                    LNK-ORCSC-KEI-IDX
                               MOVE    "K132"           TO  WRK-ERRCD
                               MOVE    "1"              TO  WRK-COM-CUR
                                                              (IDX-Z)
                               MOVE    "1"          TO  WRK-COM-KZMFLG
                                                              (IDX-Z)
                           END-IF
                       END-IF
               END-EVALUATE
             END-IF
           END-PERFORM
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      10011-TOKUYAKU-EXT
           END-IF
      *
      *    初回時
           IF      WRK-SYOKAI-YMD      =   SPACE
      *********MOVE    SPA-SYSYMD          TO  WRK-SANTEI-YMD
               MOVE    SPA-SRYYMD          TO  WRK-SANTEI-YMD
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *            特定薬剤治療管理料
                   WHEN    "113000410"
      *                初回算定年月のコメントがあり、診療年月以前の場合
                       IF      (WRK-COM-YM NOT =   SPACE )  AND
                               (WRK-COM-YM     <   WRK-SANTEI-YMD(1:6))
                           CONTINUE
                       ELSE
      *                特定薬剤治療管理加算１回目自動発生
                       MOVE    "113000770"     TO  WRK-SRYCD
                       PERFORM 1110-KASAN-CHK-SEC
                       IF      FLG-ARI         =   ZERO
                           MOVE    IDX-Z               TO  IDX-S
                           PERFORM 1100-KASAN-SET-SEC
                       ELSE
                           MOVE    IDX-T               TO  IDX-S
                       END-IF
      *                特定薬剤治療管理料初回算定追加
                       MOVE    "840000038"         TO  WRK-SRYCD
                       PERFORM 100112-COMMT1-SEC
      *
                       END-IF
      *
      *            特定薬剤治療管理料（ジギタリス製剤等）
                   WHEN    "180000110"
                   WHEN    "180000210"
      *                特定薬剤治療管理加算有無
                       MOVE    "113000770"     TO  WRK-SRYCD
                       PERFORM 1110-KASAN-CHK-SEC
                       IF      FLG-ARI         =   ZERO
                           MOVE    IDX-Z               TO  IDX-S
                       ELSE
                           MOVE    IDX-T               TO  IDX-S
                       END-IF
      *                特定薬剤治療管理料初回算定追加
                       MOVE    "840000038"         TO  WRK-SRYCD
                       PERFORM 100112-COMMT1-SEC
      *            特定薬剤治療管理料（４月目以上）はエラー
                   WHEN    "113000510"
      *!               MOVE    "0130"          TO  WRK-ERRCD
      *!               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
      *                警告へ
                       IF     ( WRK-COM-KZMFLG (IDX-Z) =   SPACE )
                          AND (LNK-ORCSC-KEI-IDX       =   ZERO  )
                           MOVE    IDX-Z            TO
                                                    LNK-ORCSC-KEI-IDX
                           MOVE    "K130"           TO  WRK-ERRCD
                           MOVE    "1"              TO  WRK-COM-CUR
                                                          (IDX-Z)
                           MOVE    "1"          TO  WRK-COM-KZMFLG
                                                          (IDX-Z)
                           GO      TO      10011-TOKUYAKU-EXT
                       ELSE
      *                    特定薬剤治療管理料初回算定追加
                           MOVE    IDX-Z               TO  IDX-S
                           MOVE    "840000038"         TO  WRK-SRYCD
                           PERFORM 100112-COMMT1-SEC
                       END-IF
               END-EVALUATE
      *
           END-IF
      *    ２回目以降
      *    Ｋ０２のみ
           IF     (WRK-SYOKAI-YMD  NOT =   SPACE  )  AND
                  (LNK-ORCSC-PG        =   "K02"  )
      *        算定回数計算
      *********PERFORM 10013-SANTEI-CNT-SEC
      *        初回算定日＋３月
               MOVE    WRK-SYOKAI-YMD      TO  WRK-SYOKAI-YMD3
      *        コメントの初回日判定
               IF     (WRK-COM-YM      NOT =   SPACE )  AND
                      (WRK-COM-YM          <   WRK-SYOKAI-YMD(1:6))
                   MOVE   WRK-COM-YM       TO  WRK-SYOKAI-YMD3(1:6)
               END-IF
               COMPUTE WRK-SYO-MM3         =   WRK-SYO-MM3   +   3
               IF      WRK-SYO-MM3         >   12
                   COMPUTE WRK-SYO-YY3     =   WRK-SYO-YY3   +   1
                   COMPUTE WRK-SYO-MM3     =   WRK-SYO-MM3   -   12
               END-IF
      *
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *            特定薬剤治療管理料
                   WHEN    "113000410"
                       IF      SPA-SRYYMD(1:6)     >=  WRK-SYOKAI-YMD3
                                                               (1:6)
      ***H14.03.08 現在 振替えなしで、警告表示
      *                   ４月目以降へ振り替え
      *                   MOVE    "113000510"          TO  WRK-SRYCD
      *                   MOVE    IDX-Z                TO  IDX-T
      *                   MOVE    1                    TO  FLG-ARI
      *                   PERFORM 1100-KASAN-SET-SEC
      *                   MOVE    SPACE            TO  WRK-COM-AUTOKBN
      *****                                                (IDX-Z)
                          IF      WRK-COM-KZMFLG (IDX-Z)   =   SPACE
                              AND (LNK-ORCSC-KEI-IDX       =   ZERO  )
                               MOVE    IDX-Z            TO
                                                   LNK-ORCSC-KEI-IDX
                               MOVE    "K007"           TO  WRK-ERRCD
                               MOVE    "1"              TO  WRK-COM-CUR
                                                           (IDX-Z)
                               MOVE    "1"         TO  WRK-COM-KZMFLG
                                                          (IDX-Z)
                           END-IF
                       END-IF
      *            特定薬剤治療管理料（４月目以上）はエラー
                   WHEN    "113000510"
                       IF      SPA-SRYYMD(1:6)     >=  WRK-SYOKAI-YMD3
                                                               (1:6)
                           CONTINUE
                       ELSE
      *                   警告へ
                          IF     (WRK-COM-KZMFLG (IDX-Z)   =   SPACE )
                             AND (LNK-ORCSC-KEI-IDX        =   ZERO  )
                              MOVE    IDX-Z            TO
                                                    LNK-ORCSC-KEI-IDX
                              MOVE    "K130"           TO  WRK-ERRCD
                              MOVE    "1"              TO  WRK-COM-CUR
                                                          (IDX-Z)
                              MOVE    "1"          TO  WRK-COM-KZMFLG
                                                          (IDX-Z)
                              GO      TO      10011-TOKUYAKU-EXT
                           END-IF
                       END-IF
               END-EVALUATE
      *
      *        特定薬剤治療管理料初回算定追加
               MOVE    WRK-SYOKAI-YMD      TO  WRK-SANTEI-YMD
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *            特定薬剤治療管理料
                   WHEN    "113000410"
      *            特定薬剤治療管理料（ジギタリス製剤等）
                   WHEN    "180000110"
                   WHEN    "180000210"
      *            特定薬剤治療管理料（４月目以上）はエラー
                   WHEN    "113000510"
                       IF      WRK-SYOKAI          =   ZERO
                           MOVE    IDX-Z               TO  IDX-S
                           MOVE    "840000038"         TO  WRK-SRYCD
                           PERFORM 100112-COMMT1-SEC
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    測定薬剤名コメント自動発生
           IF     (WRK-COM-INPUTCD (IDX-Z + 1) =   SPACE )  OR
                  (IDX-S                       >    ZERO  AND
                   WRK-COM-INPUTCD (IDX-S + 1) =   SPACE )
               MOVE    "830000014"         TO  WRK-SRYCD
               IF      IDX-S               =   ZERO
                   MOVE    IDX-Z           TO  IDX-S
               END-IF
               PERFORM 100112-COMMT1-SEC
           END-IF
      *    初回算定日をセットする
           IF      FLG-SYOKAI3         =   1
               IF     (WRK-SYOKAI-YMD      =   SPACE )  OR
                      (WRK-SYOKAI-YMD      <   SPA-SRYYMD)
                   MOVE    SPA-SRYYMD          TO  WRK-SYOKAI-YMD
               END-IF
           END-IF
      *
           IF      WRK-SYOKAI-YMD      >   LNK-ORCSC-TOKTEI-YMD
               MOVE    WRK-SYOKAI-YMD      TO  LNK-ORCSC-TOKTEI-YMD
           END-IF
      *
           .
       10011-TOKUYAKU-EXT.
           EXIT.
      *****************************************************************
      *     初回算定コメント判定処理
      *****************************************************************
       100110-COMMNT-CHK-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-COM-YM
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   )   OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE ) OR
                              (WRK-COM-YM      NOT =   SPACE )
             IF   (WRK-COM-HKNCOMBI (IDX1) =   "9999" )
               CONTINUE
             ELSE
               EVALUATE    WRK-COM-INPUTCD (IDX1)
                   WHEN    "840000038"
                       IF     (WRK-COM-ATAI1(IDX1) NOT =   SPACE ) AND
                              (WRK-COM-ATAI2(IDX1) NOT =   SPACE )
                           MOVE    WRK-COM-ATAI1(IDX1)(1:2)  TO  WRK-WYY
                           MOVE    WRK-COM-ATAI2(IDX1)(1:2)  TO  WRK-WMM
      *        入力年月を西暦変換
                           INITIALIZE                  STS-AREA-DAY
                           INITIALIZE                  LNK-DAY1-AREA
                           MOVE    "12"            TO  LNK-DAY1-IRAI
                           IF      WRK-WYY         >   "60"
                               MOVE    "3"         TO  LNK-DAY1-WYMD
                                                                (1:1)
                           ELSE
                               MOVE    "4"         TO  LNK-DAY1-WYMD
                                                                (1:1)
                           END-IF
                           MOVE    WRK-WYM     TO  LNK-DAY1-WYMD(2:4)
                           MOVE    "01"        TO  LNK-DAY1-WYMD(6:2)
                           CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                                       LNK-DAY1-AREA
                           MOVE    LNK-DAY1-YMD(1:6)   TO  WRK-COM-YM
                       END-IF
               END-EVALUATE
             END-IF
           END-PERFORM
           .
       100110-COMMNT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定薬剤治療管理料のコメント追加処理
      *****************************************************************
       100112-COMMT1-SEC           SECTION.
      *
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      100112-COMMT1-EXT
           END-IF
      *
      *****MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   ZERO
               GO      TO      100112-COMMT1-EXT
           END-IF
      *    測定薬剤名コメントの追加
           IF      WRK-SRYCD           =   "830000014"
      *        自動追加分を止める
               MOVE    SPACE               TO  WRK-COM-AUTOKBN (IDX-S)
               MOVE    "1"                 TO  WRK-COM-CUR     (IDX-S)
               GO      TO      100112-COMMT1-EXT
           END-IF
      *
      *    特定薬剤治療管理料初回算定コメント
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SANTEI-YMD      TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           MOVE    LNK-DAY1-WYMD(2:2)  TO  WRK-COM-ATAI1 (IDX-S)
           MOVE    LNK-DAY1-WYMD(4:2)  TO  WRK-COM-ATAI2 (IDX-S)
      *
      *    画面再編集
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    LNK-ORCSC-PG        TO  LNK-SC94-PG
           MOVE    IDX-S               TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    診療日を編集
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-15MIMAN-ARI   TO  LNK-SC92-15MIMAN-ARI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
           .
       100112-COMMT1-EXT.
           EXIT.
      *****************************************************************
      *    特定薬剤治療管理料算定回数計算処理
      *****************************************************************
       10013-SANTEI-CNT-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   4
               SET     TBL-TOK             TO  IDX
      *        算定履歴検索
               INITIALIZE                  SANTEI-REC
               MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
               MOVE    SPA-PTID            TO  SANTEI-PTID
               MOVE    TBL-TOKSRYCD (TBL-TOK)
                                           TO  SANTEI-SRYCD
               MOVE    WRK-SYOKAI-YMD      TO  SANTEI-FSANTEIYMD
      *        算定履歴区分は’１’であることとする
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
      *
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-SANTEI
               END-IF
               PERFORM
                       UNTIL       FLG-SANTEI      =   1
                   IF      SANTEI-MSANTEICNT   >   ZERO
                       ADD     SANTEI-MSANTEICNT   TO  WRK-SANTEI-CNT
                   END-IF
      *            訂正の時、訂正分は対象外
                   IF     (LNK-ORCSC-SYORIFLG   =   1  )  AND
                          (SANTEI-SRYYM         =   SPA-SRYYMD(1:6))
                     MOVE  ZERO                 TO  IDX-Y2
                     IF    TBL-TOKSRYCD (TBL-TOK)
                                                =   WRK-COM-INPUTCD
                                                         (IDX-Z)
                       MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                       IF      SANTEI-DAY (IDX-Y2) >   ZERO
                           COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                                   -   SANTEI-DAY
                                                             (IDX-Y2)
                       END-IF
                     END-IF
                     ADD     1                 TO  IDX-Y2
                     PERFORM VARYING     IDX-2 FROM   IDX-Y2 BY  1
                             UNTIL       IDX-2 >   31
                           COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                                   -   SANTEI-DAY
                                                             (IDX-2)
                     END-PERFORM
                   END-IF
      *            訂正の時、訂正日以降は対象外
                   IF     (LNK-ORCSC-SYORIFLG  =   1  )  AND
                          (SANTEI-SRYYM        >   SPA-SRYYMD(1:6))
                       COMPUTE WRK-SANTEI-CNT  =   WRK-SANTEI-CNT
                                               -   SANTEI-MSANTEICNT
                   END-IF
      *
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-PERFORM
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
           .
       10013-SANTEI-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴検索処理
      *****************************************************************
       100111-SANTEI-KEN-SEC                  SECTION.
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           IF      WRK-DBPATH          =   "key"
               MOVE    SPA-SRYYMD(1:6)     TO  SANTEI-SRYYM
           END-IF
      *    算定履歴区分は’１’であることとする
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    WRK-DBPATH          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    WRK-DBPATH          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100111-SANTEI-KEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   ニコチン依存管理料チェック処理
      *****************************************************************
       10014-NIKOCHIN-CHK-SEC                  SECTION.
      *
      *    初回算定日
           MOVE    "113008310"         TO  WRK-SRYCD
           PERFORM 10141-SANTEI-KEN02-SEC
           MOVE    SPACE               TO  WRK-SYO-YMD
           PERFORM VARYING     IDX-2   FROM   1    BY  1
                   UNTIL      (IDX-2   >   31  )  OR
                              (WRK-SYO-YMD NOT =   SPACE )
               IF      SANTEI-DAY (IDX-2)  >   ZERO
                   MOVE    SANTEI-SRYYM    TO  WRK-SYO-YMD(1:6)
                   MOVE    IDX-2           TO  WRK-SYO-DD
               END-IF
           END-PERFORM
           IF      WRK-SYO-YMD NOT =   SPACE
      *        初回＋12週
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-SYO-YMD         TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    84                  TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KOYOMI     TO  WRK-SYO-YMD-12
           END-IF
      *
      *    ニコチン依存管理料チェック
           EVALUATE    WRK-COM-INPUTCD (IDX-Z)
               WHEN    "113008310"
                   IF      WRK-SYO-YMD NOT =   SPACE
      *                前回の初回から１年以内 エラー
                       COMPUTE WRK-SYO-YY      =   WRK-SYO-YY
                                               +   1
                       IF      WRK-SYO-YMD     >   SPA-SRYYMD
                           IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE )
                              AND (LNK-ORCSC-KEI-IDX       =   ZERO  )
                               MOVE    IDX-Z        TO
                                                    LNK-ORCSC-KEI-IDX
                               MOVE    "K138"       TO  SPA-ERRCD
                               MOVE    "1"          TO  WRK-COM-CUR
                                                          (IDX-Z)
                               MOVE    "1"          TO  WRK-COM-KZMFLG
                                                          (IDX-Z)
                           END-IF
                       END-IF
                   END-IF
      *        ２回から４回
               WHEN    "113008410"
      *        ５回
               WHEN    "113008510"
                   IF      WRK-SYO-YMD         =   SPACE
                       MOVE    "1300"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-Z)
                   ELSE
      *                １２週以上
                       IF      WRK-SYO-YMD-12  <   SPA-SRYYMD
                           IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE )
                              AND (LNK-ORCSC-KEI-IDX       =   ZERO  )
                               MOVE    IDX-Z        TO
                                                    LNK-ORCSC-KEI-IDX
                               MOVE    "K139"       TO  SPA-ERRCD
                               MOVE    "1"          TO  WRK-COM-CUR
                                                          (IDX-Z)
                               MOVE    "1"          TO  WRK-COM-KZMFLG
                                                          (IDX-Z)
                           END-IF
                       END-IF
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "113008410"     TO  WRK-SRYCD
                       PERFORM 10143-NIKOCHIN-CHK03-SEC
                       IF      WRK-COM-INPUTCD (IDX-Z) =   "113008410"
      *                    ４回以上の算定
                           IF      WRK-SANTEI-CNT       >=  3
                               MOVE    "1301"       TO  SPA-ERRCD
                               MOVE    "1"          TO  WRK-COM-CUR
                                                          (IDX-Z)
                           END-IF
                       ELSE
      *                    ４回算定の有無
                           IF      WRK-SANTEI-CNT   NOT =   3
                               MOVE    "1302"       TO  SPA-ERRCD
                               MOVE    "1"          TO  WRK-COM-CUR
                                                          (IDX-Z)
                           ELSE
      *                    ５回目の回数チェック
                               MOVE    "113008510"     TO  WRK-SRYCD
                               PERFORM 10143-NIKOCHIN-CHK03-SEC
                               IF      WRK-SANTEI-CNT  >   ZERO
                                   MOVE    "1303"      TO  SPA-ERRCD
                                   MOVE    "1"         TO  WRK-COM-CUR
                                                          (IDX-Z)
                               END-IF
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
           .
       10014-NIKOCHIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *   小児特定疾患カウンセリング料チェック処理
      *****************************************************************
       10015-SYOUNICA-CHK-SEC          SECTION.
      *
      *    初回算定日
           MOVE    "113000810"         TO  WRK-SRYCD
           PERFORM 10141-SANTEI-KEN02-SEC
           MOVE    SANTEI-FSANTEIYMD   TO  WRK-SYO-YMD
           MOVE    SPACE               TO  WRK-LST-YMD
           PERFORM VARYING     IDX-2   FROM   1    BY  1
                   UNTIL      (IDX-2   >   31  )  OR
                              (WRK-LST-YMD NOT =   SPACE )
               IF      SANTEI-DAY (IDX-2)  >   ZERO
                   MOVE    SANTEI-SRYYM    TO  WRK-LST-YMD(1:6)
                   MOVE    IDX-2           TO  WRK-LST-DD
               END-IF
           END-PERFORM
           IF      WRK-SYO-YMD     NOT =   SPACE
      *        前回の初回から２年以上エラー
               COMPUTE WRK-SYO-YY      =   WRK-SYO-YY
                                       +   2
               IF      WRK-SYO-YMD     <=  SPA-SRYYMD
                   IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE )
                      AND (LNK-ORCSC-KEI-IDX       =   ZERO  )
                       MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                       MOVE    "K240"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z)
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                             (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *        １回目
               WHEN    "113000810"
                   IF      WRK-LST-YMD(1:6)    =   SPA-SRYYMD(1:6)
                       MOVE    "113009910"         TO  WRK-SRYCD
                       MOVE    IDX-Z               TO  IDX-T
                       MOVE    1                   TO  FLG-ARI
                       MOVE    SPACE               TO  WRK-COM-HKEIFLG
                                                          (IDX-Z)
                       MOVE    SPACE               TO  WRK-COM-HCHKFLG
                                                          (IDX-Z)
                       PERFORM 1100-KASAN-SET-SEC
                       MOVE    SPACE               TO  WRK-COM-AUTOKBN
                                                          (IDX-Z)
                   END-IF
      *        ２回目
               WHEN    "113009910"
                   IF     (WRK-LST-YMD         =   SPACE   )  OR
                          (WRK-LST-YMD(1:6)    <   SPA-SRYYMD(1:6))
                       MOVE    "113000810"         TO  WRK-SRYCD
                       MOVE    IDX-Z               TO  IDX-T
                       MOVE    1                   TO  FLG-ARI
                       MOVE    SPACE               TO  WRK-COM-HKEIFLG
                                                          (IDX-Z)
                       MOVE    SPACE               TO  WRK-COM-HCHKFLG
                                                          (IDX-Z)
                       PERFORM 1100-KASAN-SET-SEC
                       MOVE    SPACE              TO  WRK-COM-AUTOKBN
                                                          (IDX-Z)
                   END-IF
               END-EVALUATE
           END-IF
           .
       10015-SYOUNICA-CHK-EXT.
           EXIT.
      *****************************************************************
      *   算定履歴検索処理
      *****************************************************************
       10141-SANTEI-KEN02-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-SANTEI-OK
      *    外来まとめはＳＰＡに編集
           IF      LNK-ORCSC-KT01FLG   =   "1"
               PERFORM 10141-KT01-SANTEI-SEC
               IF      FLG-SANTEI-OK   =   1
                   GO      TO   10141-SANTEI-KEN02-EXT
               END-IF
           END-IF
      *
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
      *    算定履歴区分は’１’であることとする
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           PERFORM UNTIL   (FLG-SANTEI     =   1  )  OR
                           (FLG-SANTEI-OK  =   1  )
               IF      LNK-ORCSC-KT01FLG   =   "1"
      *            外来まとめは、前月分から
                   IF      SANTEI-SRYYM        <   SPA-SRYYMD(1:6)
                       MOVE    1                   TO  FLG-SANTEI-OK
                   END-IF
               ELSE
                   IF      SANTEI-SRYYM        <=  SPA-SRYYMD(1:6)
                       MOVE    1                   TO  FLG-SANTEI-OK
                   END-IF
               END-IF
      *
               IF      FLG-SANTEI-OK       =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
     *
           IF     (LNK-ORCSC-SYORIFLG  =   1  )  AND
                  (FLG-SANTEI-OK       =   1  )  AND
                  (SANTEI-SRYYM        =   SPA-SRYYMD(1:6))
      *        訂正で今回にある時
               PERFORM VARYING IDX-TEI     FROM   1  BY  1
                       UNTIL  (IDX-TEI     >   SPA-MAX-LINE )  OR
                              (LNK-ORCSC-TEI-SRYCD (IDX-TEI)
                                           =   SPACE  )
                   IF     (LNK-ORCSC-TEI-SRYCD (IDX-TEI)
                                           =   WRK-SRYCD )
                       MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
                       IF      SANTEI-DAY (IDX-Y2) >   ZERO
                           IF      SANTEI-DAY (IDX-Y2)
                                               <   LNK-ORCSC-TEI-KAISU
                                                            (IDX-TEI)
                               MOVE    ZERO        TO  SANTEI-DAY
                                                            (IDX-Y2)
                           ELSE
                               COMPUTE SANTEI-DAY(IDX-Y2)
                                               =   SANTEI-DAY (IDX-Y2)
                                               -   LNK-ORCSC-TEI-KAISU
                                                              (IDX-TEI)
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       10141-SANTEI-KEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *   算定履歴検索処理
      *****************************************************************
       10141-KT01-SANTEI-SEC        SECTION.
      *
           PERFORM VARYING     IDX-SN      FROM    1   BY  1
                   UNTIL      (IDX-SN      >   100 )   OR
                              (LNK-ORCSC-KT01-SRYCD(IDX-SN)
                                          =   SPACE)
             IF      LNK-ORCSC-KT01-SRYCD (IDX-SN)
                                          =   WRK-SRYCD
               INITIALIZE                  SANTEI-REC
               MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
               MOVE    SPA-PTID            TO  SANTEI-PTID
               MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
               MOVE    LNK-ORCSC-KT01-SRYCD (IDX-SN)
                                           TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               PERFORM VARYING     IDX-2   FROM    1   BY  1
                       UNTIL      (IDX-2   >   IDX-Y2 )
                   MOVE    LNK-ORCSC-KT01-DAY (IDX-SN IDX-2)
                                           TO  SANTEI-DAY (IDX-2)
                   ADD     SANTEI-DAY (IDX-2)    TO  SANTEI-MSANTEICNT
                   IF      SANTEI-DAY (IDX-2)    >   ZERO
                       IF      SANTEI-MSANTEID     =   SPACE
                           MOVE    IDX-2           TO  SANTEI-MSANTEID
                       END-IF
                   END-IF
               END-PERFORM
               MOVE    1               TO  FLG-SANTEI-OK
             END-IF
           END-PERFORM
           .
       10141-KT01-SANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *   算定履歴検索処理
      *****************************************************************
       10143-NIKOCHIN-CHK03-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
           MOVE    ZERO                TO  WRK-TEI-CNT
      *    外来まとめはＳＰＡに編集
           IF      LNK-ORCSC-KT01FLG   =   "1"
               PERFORM 10141-KT01-SANTEI-SEC
               PERFORM VARYING IDX-2   FROM   1      BY  1
                       UNTIL   IDX-2   >   31
                   COMPUTE WRK-SANTEI-CNT
                                       =   WRK-SANTEI-CNT
                                       +   SANTEI-DAY (IDX-2)
               END-PERFORM
           END-IF
      *
           IF      LNK-ORCSC-SYORIFLG  =   1
      *        訂正で今回にある時
               PERFORM VARYING IDX-TEI     FROM   1  BY  1
                       UNTIL  (IDX-TEI     >   SPA-MAX-LINE)  OR
                              (LNK-ORCSC-TEI-SRYCD (IDX-TEI)
                                           =   SPACE  )
                   IF     (LNK-ORCSC-TEI-SRYCD (IDX-TEI)
                                           =   WRK-SRYCD )
                       MOVE    LNK-ORCSC-TEI-KAISU (IDX-TEI)
                                           TO  WRK-TEI-CNT
                   END-IF
               END-PERFORM
           END-IF
      *
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
      *    算定履歴区分は’１’であることとする
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           PERFORM UNTIL   (FLG-SANTEI     =   1  )
               IF      SANTEI-SRYYM        <   WRK-SYO-YMD(1:6)
                   MOVE    1                   TO  FLG-SANTEI
               ELSE
      *        外来まとめは当月以外
               IF     (LNK-ORCSC-KT01FLG   =   "1" ) AND
                      (SANTEI-SRYYM        =   SPA-SRYYMD(1:6))
                   CONTINUE
               ELSE
                   IF      LNK-ORCSC-SYORIFLG  =   1
      *               訂正時は訂正日までの件数
                      IF      SANTEI-SRYYM        <=  SPA-SRYYMD(1:6)
                           IF      SANTEI-SRYYM   =   SPA-SRYYMD(1:6)
                               MOVE    SPA-SRYYMD(7:2)    TO  IDX-Y2
                           ELSE
                               MOVE    31                 TO  IDX-Y2
                           END-IF
                           PERFORM VARYING IDX-2   FROM   1      BY  1
                                   UNTIL   IDX-2   >   IDX-Y2
                               COMPUTE WRK-SANTEI-CNT
                                                   =   WRK-SANTEI-CNT
                                                   +   SANTEI-DAY
                                                             (IDX-2)
                           END-PERFORM
                       END-IF
                   ELSE
                       PERFORM VARYING IDX-2   FROM   1      BY  1
                               UNTIL   IDX-2   >   31
                           COMPUTE WRK-SANTEI-CNT
                                               =   WRK-SANTEI-CNT
                                               +   SANTEI-DAY
                                                             (IDX-2)
                       END-PERFORM
                   END-IF
               END-IF
      *
               END-IF
               IF      FLG-SANTEI              =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
     *
           COMPUTE WRK-SANTEI-CNT      =   WRK-SANTEI-CNT
                                       -   WRK-TEI-CNT
      *
           .
       10143-NIKOCHIN-CHK03-EXT.
           EXIT.
      *
      *****************************************************************
      *   コメントのチェック処理
      *****************************************************************
       10018-COMMENT-SEC                  SECTION.
      *
      *    包括まとめ入力は以下対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               OR (WRK-COM-HKNCOMBI (IDX-Z) =   "9999" )
               GO      TO      10018-COMMENT-EXT
           END-IF
      *
      *    特定薬剤治療管理初回算定コメントがあれば初回とする
           IF     (WRK-COM-INPUTCD (IDX-Z) =   "840000038" ) AND
                  (LNK-ORCSC-TOKTEI-YMD    NOT =   SPACE   )
      *        特定開始日和暦変換
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "11"                    TO  LNK-DAY1-IRAI
               MOVE    LNK-ORCSC-TOKTEI-YMD    TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
      *
               MOVE    WRK-COM-ATAI1 (IDX-Z)(1:2)  TO  WRK-WYY
               MOVE    WRK-COM-ATAI2 (IDX-Z)(1:2)  TO  WRK-WMM
      *        初回算定年月が前回初回と違う時システム日を初回算定とする
               IF      WRK-WYM             =   LNK-DAY1-WYMD(2:4)
                   CONTINUE
               ELSE
      *            診療年月和暦変換
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY1-AREA
                   MOVE    "11"                    TO  LNK-DAY1-IRAI
                   MOVE    SPA-SRYYMD              TO  LNK-DAY1-YMD
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY1-AREA
                   IF      WRK-WYM         =   LNK-DAY1-WYMD(2:4)
                       MOVE    SPA-SRYYMD      TO  LNK-ORCSC-TOKTEI-YMD
                   ELSE
      *                警告とする
                       IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE )
                          AND (LNK-ORCSC-KEI-IDX       =   ZERO  )
                           MOVE    IDX-Z            TO
                                                    LNK-ORCSC-KEI-IDX
                           MOVE    "K133"           TO  WRK-ERRCD
                           MOVE    "1"              TO  WRK-COM-CUR
                                                          (IDX-Z)
                           MOVE    "1"          TO  WRK-COM-KZMFLG
                                                          (IDX-Z)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    特定薬剤治療管理初回算定コメントがあれば初回とする
           IF     (WRK-COM-INPUTCD (IDX-Z) =   "840000038" ) AND
                  (LNK-ORCSC-TOKTEI-YMD    =   SPACE   )  AND
                  (WRK-COM-ATAI1 (IDX-Z)(1:2)  NOT =   SPACE) AND
                  (WRK-COM-ATAI2 (IDX-Z)(1:2)  NOT =   SPACE )
      *
               MOVE    WRK-COM-ATAI1 (IDX-Z)(1:2)  TO  WRK-WYY
               MOVE    WRK-COM-ATAI2 (IDX-Z)(1:2)  TO  WRK-WMM
      *        入力年月を西暦変換
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "12"                TO  LNK-DAY1-IRAI
               IF      WRK-WYY                 >   "60"
                   MOVE    "3"                 TO  LNK-DAY1-WYMD(1:1)
               ELSE
                   MOVE    "4"                 TO  LNK-DAY1-WYMD(1:1)
               END-IF
               MOVE    WRK-WYM             TO  LNK-DAY1-WYMD(2:4)
               MOVE    "01"                TO  LNK-DAY1-WYMD(6:2)
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               IF      SPA-SRYYMD(1:6)     >   WRK-SYMD(1:6)
                   MOVE    WRK-SYMD            TO  LNK-ORCSC-TOKTEI-YMD
               ELSE
                   IF      SPA-SRYYMD(1:6)     <   WRK-SYMD(1:6)
                       MOVE    "0135"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *
      *    ＤＵＭＭＹの診察料判定
      *    Ｋ０２のみ
           IF      LNK-ORCSC-PG        =   "K02"
               CONTINUE
           ELSE
               GO      TO      10018-COMMENT-EXT
           END-IF
           IF      WRK-COM-INPUTCD (IDX-Z) =   "820000003"  OR
                                               "820000004"  OR
                                               "820000005"  OR
                                               "820000006"
               IF      WRK-COM-SRYKBN(IDX-Z)   =   "11"  OR  "12"
      *            同じ剤内にＤＵＭＭＹがない時自動発生する
                   PERFORM 100181-DUMMY-SYORI-SEC
               ELSE
                   MOVE    "0136"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
      *
           .
       10018-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＵＭＭＹ診察料自動発生処理
      *****************************************************************
       100181-DUMMY-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE   )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE  AND
                               WRK-COM-INPUTCD (IDX)   =   SPACE )
             IF   (WRK-COM-HKNCOMBI (IDX)  =   "9999" )
               CONTINUE
             ELSE
               IF      WRK-GMN-INPUTCD(IDX)    =   ".110"  OR
                                                   ".120"
                   MOVE    1                   TO  FLG-OK
               END-IF
               IF      WRK-COM-INPUTCD (IDX)   =   "099110001"  OR
                                                   "099120001"
                   MOVE    1                   TO  FLG-OK
               END-IF
             END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0136"          TO  WRK-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (01)
               GO      TO      100181-DUMMY-SYORI-EXT
           END-IF
      *
      *
           IF      WRK-COM-SRYKBN (IDX-Z)  =   "11"
      *        初診  同日診察でないこと
               IF     (LNK-ORCSC-SYORIFLG  =   ZERO       )  AND
                      (LNK-ORCSC-LSTYMD    =   SPA-SRYYMD )  AND
                      (LNK-ORCSC-SYOYMD    NOT =   LNK-ORCSC-LSTYMD)
                   MOVE    "0138"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (01)
               END-IF
           ELSE
      *        再診  初診料が算定済みであること
               IF     (LNK-ORCSC-SYORIFLG  =   ZERO       )  AND
                      (LNK-ORCSC-LSTYMD    =   SPACE      )
                   MOVE    "0139"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (01)
               END-IF
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      100181-DUMMY-SYORI-EXT
           END-IF
      *    診察料自動算定
           IF      WRK-COM-SRYKBN (IDX-Z)  =   "11"
               MOVE    "099110001"         TO  WRK-SRYCD
           ELSE
               MOVE    "099120001"         TO  WRK-SRYCD
           END-IF
           PERFORM 1110-KASAN-CHK-SEC
           IF      FLG-ARI             =   1
               GO      TO      100181-DUMMY-SYORI-EXT
           END-IF
           MOVE    IDX-Z               TO  IDX-S
           PERFORM 1100-KASAN-SET-SEC
           IF      FLG-ADD             =   1
      *        自動追加分を止める
               MOVE    SPACE               TO  WRK-COM-AUTOKBN (IDX-S)
           END-IF
      *
           .
       100181-DUMMY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＵＭＭＹ診察料判定処理
      *****************************************************************
       10012-DUMMYSYORI-SEC          SECTION.
      *
      *    Ｋ０２のみ
           IF      LNK-ORCSC-PG        =   "K02"
               CONTINUE
           ELSE
               GO      TO      10012-DUMMYSYORI-EXT
           END-IF
      *
      *    ＤＵＭＭＹの診察料判定
           EVALUATE    WRK-COM-INPUTCD (IDX-Z)
               WHEN    "099110001"
      *            初診  同日診察でないこと
                   IF     (LNK-ORCSC-SYORIFLG  =   ZERO       )  AND
                          (LNK-ORCSC-LSTYMD    =   SPA-SRYYMD )  AND
                          (LNK-ORCSC-SYOYMD    NOT =   LNK-ORCSC-LSTYMD)
                       MOVE    "0138"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               WHEN    "099120001"
      *            再診  初診料が算定済みであること
                   IF     (LNK-ORCSC-SYORIFLG  =   ZERO       )  AND
                          (LNK-ORCSC-LSTYMD    =   SPACE      )
                       MOVE    "0139"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               WHEN    "099140001"
      *            寝たきり老人在宅診察料の算定終了
                   IF     (LNK-ORCSC-SYORIFLG  =   ZERO      )  AND
                          (SPA-NETAKIRI-ARI    =   ZERO      )  AND
                          (SPA-NETAKIRI-ARI2   =   ZERO      )  AND
                          (SPA-NETAKIRI-ARI4   =   ZERO      )
                       MOVE    "0142"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
           END-EVALUATE
      *
           .
       10012-DUMMYSYORI-EXT.
           EXIT.
      *****************************************************************
      *    注加算コードの順チェック処理
      *****************************************************************
       10012-CHUKSNCD-CHK-SEC          SECTION.
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (WRK-ERRCD   NOT =   SPACE )  OR
                             ((WRK-COM-INPUTCD (IDX-Z) =   SPACE ) AND
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE ))
               IF     (WRK-COM-INPUTCD  (IDX-Z)(1:1)   =   "1"  )  AND
                      (WRK-COM-CHUKSNCD (IDX-Z)    NOT =   ZERO )
                   COMPUTE IDX-Z2              =   IDX-Z   +   1
                   MOVE    ZERO                TO  IDX-ERR
                   MOVE    ZERO                TO  IDX-ECHK
                   PERFORM VARYING     IDX     FROM    IDX-Z2  BY  1
                           UNTIL      (IDX         >   SPA-MAX-LINE)
                                  OR  (IDX-ERR     NOT =   ZERO )
                       IF     (WRK-COM-INPUTCD  (IDX)(1:1)   =   "1")
                           IF      WRK-COM-CHUKSNCD (IDX-Z)
                                              =  WRK-COM-CHUKSNCD (IDX)
                               IF      IDX-ECHK        NOT =   ZERO
                                   MOVE    IDX             TO  IDX-ERR
                               END-IF
                           ELSE
                               MOVE    IDX             TO  IDX-ECHK
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      IDX-ERR             >   ZERO
      *                注加算コードの違う加算が間に存在する
                       IF     (WRK-COM-KZMFLG (IDX-ERR)
                                                   =   SPACE )  AND
                              (LNK-ORCSC-KEI-IDX   =   ZERO )
                           MOVE    IDX-ERR     TO  LNK-ORCSC-KEI-IDX
                           MOVE    "K025"      TO  WRK-ERRCD
                           MOVE    "1"         TO  WRK-COM-CUR
                                                             (IDX-ERR)
                           MOVE    "1"         TO  WRK-COM-KZMFLG
                                                             (IDX-ERR)
                           MOVE    WRK-COM-NAME (IDX-ERR)
                                               TO  LNK-ORCSC-ERRMSG
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       10012-CHUKSNCD-CHKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算編集処理
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-SYU
           MOVE    1                   TO  IDX-STR
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSC90AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE )   OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                             ((WRK-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (WRK-COM-INPUTCD (IDX-Z) =   SPACE ))
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-GMN-KEI   (IDX-Z)
      *        手技毎
               INITIALIZE                   WRK-COM-PLUS-AREA(IDX-Z)
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."  OR "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    最終行
      *    割合計算加算
           IF      IDX-SYU             =   ZERO
               MOVE    1                   TO  IDX-SYU
           END-IF
           PERFORM 1006-KASAN-SET-SEC
      *    合計点数計算・編集
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    IDX-SYU             TO  LNK-SC90-IDX
           MOVE    IDX-STR             TO  LNK-SC90-IDX-STR
           MOVE    IDX-MAX             TO  LNK-SC90-IDX-END
           CALL    "ORCSC90TEN"        USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *    計
           COMPUTE WRK-COM-KEI (IDX-MAX)   =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
      *
      *    院外分の時、ゼロとする
           IF      WRK-COM-SRYSYUKBN(IDX-MAX)  =   "148"  OR
                                                   "149"
               MOVE    ZERO            TO  WRK-COM-TENSU  (IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-SYUTEN1 (IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-KIZAITEN(IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-YKZTEN (IDX-MAX)
               MOVE    ZERO            TO  WRK-COM-KEI    (IDX-MAX)
           END-IF
           .
       1002-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       10021-KEISAN-SEC                  SECTION.
      *
           IF     (WRK-COM-INPUTCD(IDX-Z)(1:1) =   "1" ) AND
                 ((WRK-COM-DATAKBN(IDX-Z)      =   "1" )  OR
                  (WRK-COM-KOKUJISKBKBN1(IDX-Z)    =   1
                                                   OR  3
                                                   OR  5 ))
      *        点数を個別に記載
               IF      IDX-SYU             >   ZERO
      *            割合加算
                   PERFORM 1006-KASAN-SET-SEC
      *            点数計算
                   PERFORM 1007-TENSU-SUM-SEC
               END-IF
               MOVE    IDX-Z               TO  IDX-SYU
           END-IF
      *
      *    手技点数計算
           EVALUATE    WRK-MOJI
               WHEN    "1"
                   PERFORM 100211-SYUGITEN-KEI-SEC
           END-EVALUATE
      *    回数
           MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU (IDX-Z)
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技点数計算処理
      *****************************************************************
       100211-SYUGITEN-KEI-SEC              SECTION.
      *
           IF      WRK-COM-TENSIKIBETU (IDX-Z)     =   5
                                                   OR  6
               CONTINUE
           ELSE
      *        往診料の時、往診区分により、点数集計を変更する
               IF     (WRK-COM-SRYKBN      (IDX-Z)     =   "14" )  AND
                      (WRK-COM-TUSOKUGAIKBN(IDX-Z) NOT =   "1"  )  AND
                      (WRK-COM-OSINKBN     (IDX-Z) NOT =   ZERO )
                   EVALUATE    WRK-COM-OSINKBN (IDX-Z)
                   WHEN    1
                   WHEN    2
      *            特別往診料
                   WHEN    5
                   WHEN    6
                       ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-TENSU-O1
                   WHEN    OTHER
                       ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-TENSU-O2
                   END-EVALUATE
               ELSE
      *        通則加算対象外区分判定
                   IF      WRK-COM-TUSOKUGAIKBN(IDX-Z) =   "1"
                       ADD     WRK-COM-KISOTEN (IDX-Z) TO  WRK-TENSU-GAI
                   ELSE
                       ADD     WRK-COM-KISOTEN(IDX-Z)  TO  WRK-TENSU-KEI
                   END-IF
               END-IF
           END-IF
           .
       100211-SYUGITEN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算計算処理
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-GEN
           PERFORM VARYING     IDX     FROM    IDX-SYU BY  1
                   UNTIL      (IDX         >=  IDX-Z       )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )
                   IF      WRK-COM-OSINKBN (IDX)   =   ZERO
                       COMPUTE WRK-TENSUKEI    =   WRK-TENSU-KEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
      *                往診以外
                       MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ELSE
      *                往診
                       COMPUTE WRK-TENSUKEI    =   WRK-TENSU-O1
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                       MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   END-IF
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
      *
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6   )
                   COMPUTE WRK-TENSUKEI        =   WRK-TENSU-KEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-GEN
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSU      =   WRK-TENSU-KEI
                                       +   WRK-TENSU-GAI
                                       +   WRK-TENSU-O1
                                       +   WRK-TENSU-O2
                                       +   WRK-TENSUKEI-G
                                       -   WRK-TENSUKEI-GEN
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    手技毎の計算処理
      *****************************************************************
       1007-TENSU-SUM-SEC            SECTION.
      *
           MOVE    "1"                 TO  LNK-SC90-KBN
           MOVE    IDX-SYU             TO  LNK-SC90-IDX
           MOVE    IDX-STR             TO  LNK-SC90-IDX-STR
           COMPUTE LNK-SC90-IDX-END    =   IDX-Z   -   1
           CALL    "ORCSC90TEN"        USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
           MOVE    IDX-Z               TO  IDX-STR
           INITIALIZE                  WRK-TENSU-KEISAN
           MOVE    ZERO                TO  LNK-SC90-TENSU
           .
       1007-TENSU-SUM-EXT.
           EXIT.
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC               SECTION.
      *
      *    重複チェック
           IF      LNK-ORCSC-PG        =   "K02"
      *
           PERFORM 2000-JYUFUKU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-ALL-SYORI-EXT
           END-IF
      *
           END-IF
      *
      *    包括まとめ入力は対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      200-ALL-SYORI-EXT
           END-IF
      *
           IF      LNK-ORCSC-PG        =   "K02"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           ELSE
               INITIALIZE                      SPA-K01KYOUTU
           END-IF
      *
           MOVE    ZERO                TO  IDX-ZAI
           MOVE    ZERO                TO  IDX-ZAI1
           MOVE    ZERO                TO  IDX-ZAI2
           MOVE    ZERO                TO  IDX-SYU
           MOVE    ZERO                TO  IDX-SYU2
           MOVE    ZERO                TO  FLG-GAIKAN
           MOVE    ZERO                TO  IDX-GAIKAN
           MOVE    ZERO                TO  LNK-ORCSC-CHK-IDX
      *
           MOVE    ZERO                TO  FLG-SYOKAI
           MOVE    ZERO                TO  FLG-SYOKAI2
           MOVE    ZERO                TO  FLG-TAHOKEN
      *
           MOVE    ZERO                TO  FLG-KENSIN
           MOVE    ZERO                TO  FLG-DUMMY-ARI
           MOVE    ZERO                TO  FLG-JIKASAN
      *
           MOVE    SPACE               TO  WRK-KEICD
           MOVE    ZERO                TO  WRK-KEICD-IDX
           MOVE    ZERO                TO  WRK-RSISYOSIN
           MOVE    ZERO                TO  IDX-SINSATU
           MOVE    LNK-ORCSC-TOKUTEI-JNZ
                                       TO  WRK-TOKUTEI-JNZ
           MOVE    LNK-ORCSC-TOKUTEI-KNJ
                                       TO  WRK-TOKUTEI-KNJ
           MOVE    LNK-ORCSC-TOKUTEIG-KNJ
                                       TO  WRK-TOKUTEIG-KNJ
           MOVE    ZERO                TO  LNK-ORCSC-TOKUTEI-JNZ
           MOVE    ZERO                TO  LNK-ORCSC-TOKUTEI-KNJ
           MOVE    ZERO                TO  LNK-ORCSC-TOKUTEIG-KNJ
           MOVE    ZERO                TO  LNK-ORCSC-GAIRAI-TEN
      *
           MOVE    ZERO                TO  FLG-TAKA
           MOVE    ZERO                TO  FLG-TAKA-2
           MOVE    ZERO                TO  FLG-DOUJITU
           MOVE    ZERO                TO  IDX-SINSATU2
      *    複数科・保険
           MOVE    ZERO                TO  FLG-FUKUSU
      *    他医院退院日
           MOVE    ZERO                TO  FLG-TAITAIIN
      *    診察料チェック
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE      )  OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *!!!
      *              来まとめは診察料チェックなし
                   OR        (LNK-ORCSC-PG        =   "KT01" )
      *!!!
             IF   (WRK-COM-HKNCOMBI (IDX-Z) =   "9999" )
      *          治験減点分
             OR   (WRK-COM-GENTEN (IDX-Z)   =   "1")
               CONTINUE
             ELSE
      *        診察料の判定
               IF      IDX-SINSATU         =   ZERO
                   PERFORM 2001-SINSATU-CHK-SEC
                   IF     (FLG-TBL             =   1   )
                       MOVE    WRK-KBN         TO  WRK-SINSATU-KBN
                       MOVE    WRK-KAISU       TO  WRK-SINSATU-KAISU
                       MOVE    WRK-KBN2        TO  WRK-SINSATU-KBN2
                       MOVE    IDX-Z           TO  IDX-SINSATU
                   END-IF
                   IF     (WRK-KBN             =   "F" )  AND
                          (WRK-KBN2            =   "D" )
                       MOVE    1               TO  FLG-DOUJITU
                       MOVE    IDX-Z           TO  IDX-SINSATU2
                   END-IF
      *            最初の診察料が複数科・保険の時
                   IF     (FLG-TBL             =   1   )
                   AND    (FLG-FUKUSU          =   1   )
                       IF     (WRK-KBN             =   "F" )  AND
                              (WRK-KBN2        NOT =   "D" )
                           MOVE    "1044"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z)
                       ELSE
      *                    初診料以外は警告
                           IF     (WRK-COM-KZMFLG (IDX-Z)
                                                       =   SPACE ) AND
                                  (LNK-ORCSC-KEI-IDX   =   ZERO )
                           MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                           MOVE    "K144"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z)
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                             (IDX-Z)
                           END-IF
                       END-IF
                   END-IF
      *            労災の時、労災コードで判定する
                   IF      SPA-HKNKBN      =   1
      *DD            AND  (SPA-RSI-HKNKBN  NOT =   "5" )
                     AND  (SPA-RSI-JUNKYO  NOT =   "1" )
                       PERFORM 20012-RSISINSATU-CHK-SEC
                   END-IF
               ELSE
                   PERFORM 2001-SINSATU-CHK-SEC
                   IF     (WRK-KBN             =   "F" )  AND
                          (WRK-KBN2            =   "D" )
                       MOVE    1               TO  FLG-DOUJITU
                       MOVE    IDX-Z           TO  IDX-SINSATU2
                   END-IF
      *            労災の時、労災コードで判定する
                   IF      SPA-HKNKBN      =   1
                     AND  (SPA-RSI-JUNKYO  NOT =   "1" )
                       PERFORM 20012-RSISINSATU-CHK-SEC
                   END-IF
               END-IF
      *
      *        外来管理加算、継続管理加算の判定
               PERFORM 100111-KEIZOKU-CHK-SEC
      *        労災コードの判定
               PERFORM 200112-RSI-KEI-CHK-SEC
      *
      *        外来管理加算、継続加算以外のチェック
               IF    ( FLG-GAIRAI              =    1   OR
                       FLG-KEIZOK              =    1   OR
      *                労災分以外
                       FLG-RSIFLG              =    1         )
                   CONTINUE
               ELSE
                   IF      WRK-COM-DATAKBN(IDX-Z)  =    "1"
                       IF      WRK-COM-SRYKBN(IDX-Z)   =    "11"
                           ADD     1               TO  IDX-ZAI1
                           IF      IDX-Z           =   IDX-SINSATU2
                               CONTINUE
                           ELSE
                               MOVE   IDX-Z            TO  IDX-SYU2
                           END-IF
                       END-IF
                       IF      WRK-COM-SRYKBN(IDX-Z)   =    "12"
                           ADD     1               TO  IDX-ZAI2
                           MOVE   IDX-Z            TO  IDX-SYU2
                       END-IF
                   ELSE
      *                他科にて受診済みも診察料とする
                       IF     (WRK-COM-INPUTCD(IDX-Z)  =  "830000021")
                         AND  (WRK-COM-SRYKBN (IDX-Z)  =  "12"      )
                           ADD     1               TO  IDX-ZAI2
                           IF     (IDX-ZAI2        >   1   )   AND
                                  (WRK-COM-SRYKA(IDX-Z)
                                               NOT =   SPA-SRYKA)
                               COMPUTE IDX-ZAI2    =   IDX-ZAI2  -  1
                               MOVE    1               TO  FLG-TAKA-2
                           ELSE
                               MOVE   IDX-Z            TO  IDX-SYU2
                               MOVE    1               TO  FLG-TAKA
                           END-IF
                       END-IF
                       IF     (WRK-COM-INPUTCD(IDX-Z)  =  "830000020")
                         AND  (WRK-COM-SRYKBN (IDX-Z)  =  "11"      )
                           ADD     1               TO  IDX-ZAI1
                           IF     (IDX-ZAI1        >   1   )   AND
                                  (WRK-COM-SRYKA(IDX-Z)
                                               NOT =   SPA-SRYKA)
                               COMPUTE IDX-ZAI1    =   IDX-ZAI1  -  1
                               MOVE    1               TO  FLG-TAKA-2
                           ELSE
                               MOVE   IDX-Z            TO  IDX-SYU2
                               MOVE    1               TO  FLG-TAKA
                           END-IF
                       END-IF
      *                診察料ＤＵＭＭＹも診察料とする
                       IF     (WRK-COM-INPUTCD(IDX-Z)  =  "099110001")
                         AND  (WRK-COM-SRYKBN (IDX-Z)  =  "11"      )
                           ADD     1               TO  IDX-ZAI1
                           MOVE   IDX-Z            TO  IDX-SYU2
                       END-IF
                       IF     (WRK-COM-INPUTCD(IDX-Z)  =  "099120001")
                         AND  (WRK-COM-SRYKBN (IDX-Z)  =  "12"      )
                           ADD     1               TO  IDX-ZAI2
                           MOVE   IDX-Z            TO  IDX-SYU2
                       END-IF
                   END-IF
               END-IF
      *
               IF     (WRK-COM-SRYKBN (IDX-Z)  =   "11"  OR "12" ) AND
                      (WRK-COM-DATAKBN(IDX-Z)  =   "1"           )
      *            外来管理加算、継続加算、同日再診、同日電話再診
                   IF      (FLG-GAIRAI         =   1  )  OR
                           (FLG-KEIZOK         =   1  )  OR
                           (FLG-RSIFLG         =   1  )  OR
                           (WRK-SINSATU-KBN    =   "D"   OR
                                                   "S" )
                      OR   (FLG-DOUJITU        =   1  )
      *!!!
                      OR   (LNK-ORCSC-PG       =   "KT01"  )
      *!!!
                       CONTINUE
                   ELSE
                       IF      LNK-ORCSC-CHK-IDX   >   ZERO
                           MOVE    "0017"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-Z)
                       ELSE
                           MOVE    IDX-Z       TO  LNK-ORCSC-CHK-IDX
                       END-IF
                   END-IF
               END-IF
      *        併用算定チェック
      *?0608   IF      WRK-COM-SDOKNRYO (IDX-Z)    NOT =   ZERO
      *************PERFORM 220-SDOKNRYO-CHK-SEC
      *???     END-IF
      *
      *        外来管理加算算定チェック
               IF     (WRK-COM-SRYKBN (IDX-Z)  =    "12" )  AND
                      (WRK-COM-DATAKBN(IDX-Z)  =    "1" )
                   MOVE    IDX-Z               TO  IDX-SYU
               END-IF
      *
      *        外来管理加算算定
               IF     FLG-GAIRAI          =   1
      *            外来管理加算は１つのみ
                   IF      IDX-GAIKAN          >   ZERO
                       MOVE    "0058"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   ELSE
                       MOVE    IDX-Z           TO  IDX-GAIKAN
                    END-IF
                END-IF
                IF     (WRK-COM-SRYKBN     (IDX-Z)
                                           NOT =   "95" AND "96") AND
                       (WRK-COM-GAIKANRIKBN(IDX-Z)
                                               =   1   )
                   MOVE    1               TO  FLG-GAIKAN
                END-IF
      *
      *        他保険算定済み
               IF      WRK-COM-INPUTCD(IDX-Z) =   "099999902"
                   IF      WRK-COM-HKNCOMBI(IDX-Z)
                                               =   SPA-HKNCOMBINUM
                       MOVE    1           TO  FLG-TAHOKEN
                   END-IF
               END-IF
      *        小児科外来診療科チェック
      *            初診
               IF      WRK-COM-INPUTCD(IDX-Z) =   "113003510"  OR
                                                  "113003710"
                   ADD     1               TO  IDX-ZAI1
                   MOVE   IDX-Z            TO  IDX-SYU2
               END-IF
      *            再診
               IF      WRK-COM-INPUTCD(IDX-Z) =   "113003610"  OR
                                                  "113003810"
                   ADD     1               TO  IDX-ZAI2
                   MOVE   IDX-Z            TO  IDX-SYU2
               END-IF
      *
      *        検診からのコメント
               IF      WRK-COM-SRYKBN (IDX-Z) =   "11"  OR  "12"
                   IF      WRK-COM-INPUTCD(IDX-Z)  =  "820000003"  OR
                                                      "820000004"  OR
                                                      "820000005"  OR
                                                      "820000006"
                       MOVE    1                   TO  FLG-KENSIN
                   END-IF
                   IF      WRK-COM-INPUTCD(IDX-Z)  =  "099110001"  OR
                                                      "099120001"
                       IF      FLG-DUMMY-ARI       =   1
                           MOVE    "0137"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-Z)
                       ELSE
                           MOVE    1                   TO  FLG-DUMMY-ARI
                       END-IF
                   END-IF
               END-IF
      *
      *        注加算コードのチェック（すべての診区で行う）
               IF      (WRK-COM-SRYKBN     (IDX-Z)
                                           =   "95" OR  "96"
                                                    OR  "99")   OR
                       (WRK-COM-CHUKSNCD   (IDX-Z)
                                           =   SPACE  OR  ZERO )  OR
                       (WRK-COM-CHUKSNTUBAN(IDX-Z)
                                           =   SPACE  OR  ZERO)
                   CONTINUE
               ELSE
                   PERFORM 290-CHUKSNCHK-SEC
               END-IF
      *
      *        時間外加算チェック（診察、加算、時間加算のもの）
               IF     (WRK-COM-SRYKBN  (IDX-Z)     =   "11"  OR
                                                       "12"  OR
                                                       "13"     )
                   IF     (WRK-COM-DATAKBN   (IDX-Z)   =   "2"  ) AND
                          (WRK-COM-TIMEKSNKBN(IDX-Z)   NOT =   ZERO)
                       MOVE    1               TO  FLG-JIKASAN
                   END-IF
                   IF     (WRK-COM-DATAKBN (IDX-Z)   =   "1"  ) AND
                          (WRK-COM-TIMEFLG (IDX-Z)   NOT =   ZERO )
                       MOVE    1               TO  FLG-JIKASAN
                   END-IF
               END-IF
      *
      *        他医院退院日
               IF      WRK-COM-INPUTCD (IDX-Z) =   "099999906"
                   MOVE    1                   TO  FLG-TAITAIIN
               END-IF
      *
             END-IF
      *        複数科・保険
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1) =   "#"
                                                   OR  "$"
                    MOVE    1              TO  FLG-FUKUSU
               END-IF
      *
           END-PERFORM
      *
      *    診察料以外チェック
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE      )  OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                             ((WRK-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (WRK-COM-INPUTCD (IDX-Z) =   SPACE ))
             IF     (WRK-COM-SRYKBN (IDX-Z)    =   "96"
                                               OR  "95" )
                OR  (WRK-COM-HKNCOMBI (IDX-Z) =   "9999" )
      *    以下外来まとめはチェックなし
                OR     (LNK-ORCSC-PG        =   "KT01" )
                CONTINUE
             ELSE
      *         指導料・初診日チェック
                IF      WRK-COM-INPUTCD(IDX-Z) =   "113001810"  OR
                                                   "113001910"  OR
                                                   "113002010"  OR
                                                   "113700610"  OR
                                                   "113700710"  OR
                                                   "113700810" 
                   MOVE    ZERO                TO  FLG-SIDOU
                   PERFORM 230-SIDOU-SYOSIN-SEC
               END-IF
      *        皮膚科特定疾患
               IF      WRK-COM-INPUTCD(IDX-Z) =    "113002310"
                                               OR  "113000910"
                   MOVE    1                   TO  FLG-SIDOU
                   PERFORM 230-SIDOU-SYOSIN-SEC
               END-IF
      *
      *        難病外来指導管理料
               IF      WRK-COM-INPUTCD(IDX-Z)  =   "113002910"
      *        小児指導管理料
                                               OR  "113002210"
      *        てんかん指導管理料
                                               OR  "113002850"
      *        耳鼻咽喉科特定疾患指導管理料
                                               OR  "113010110"
                   MOVE    1                   TO  FLG-SIDOU
                   PERFORM 230-SIDOU-SYOSIN-SEC
               END-IF
      *        ニコチン依存管理料チェック
               IF      WRK-COM-INPUTCD (IDX-Z) =   "113008310"
                                               OR  "113008410"
                                               OR  "113008510"
                   PERFORM 10014-NIKOCHIN-CHK-SEC
               END-IF
      *        入院中の回数
      *        リンパ浮腫指導管理料
               IF      WRK-COM-INPUTCD (IDX-Z) =   "113010410"
                   MOVE    1                   TO  FLG-NYUIN
                   MOVE    1                   TO  WRK-NYUCNT
                   PERFORM 10016-NYUIN-KIKAN-SEC
               END-IF
      *        退院時共同指導料２
               IF      WRK-COM-INPUTCD (IDX-Z) =   "113008910"
                   MOVE    2                   TO  FLG-NYUIN
                   MOVE    2                   TO  WRK-NYUCNT
                   PERFORM 10016-NYUIN-KIKAN-SEC
               END-IF
      *
      *        特定薬剤治療管理初回算定コメントチェック
               IF      WRK-COM-INPUTCD (IDX-Z) =   "840000038"
      *            特定開始日和暦変換
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY1-AREA
                   MOVE    "11"                TO  LNK-DAY1-IRAI
                   MOVE    SPA-SRYYMD          TO  LNK-DAY1-YMD
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY1-AREA
                   MOVE    WRK-COM-ATAI1 (IDX-Z)(1:2)  TO  WRK-WYY
                   MOVE    WRK-COM-ATAI2 (IDX-Z)(1:2)  TO  WRK-WMM
                   IF      WRK-WYM             =   LNK-DAY1-WYMD(2:4)
                       MOVE    1                   TO  FLG-SYOKAI
                   END-IF
               END-IF
             END-IF
           END-PERFORM
      *
      *    時間外加算幼児チェック
           MOVE    ZERO                TO  LNK-ORCSC-JIKANERR-FLG
           IF     (LNK-ORCSC-SYORIFLG      =   ZERO) AND
                  (SPA-NYUGAIKBN           =   "2" ) AND
                  (SPA-SRYYMD              =   SPA-SYSYMD) AND
                  (LNK-ORCSC-SINSATU-ARI   =   1  )  AND
                  (LNK-ORCSC-NENREI-YY     <   6  )  AND
                  (LNK-ORCSC-JIKANTOKU-NYU =   1  )  AND
                  (FLG-JIKASAN             =   ZERO)
      *        時間外算定可能で、時間外が無い時
               IF      SPA-JIKANCHK-ERR        =   "1"
                   MOVE    1               TO  LNK-ORCSC-JIKANERR-FLG
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      200-ALL-SYORI-EXT
           END-IF
      *!!!
      *    以下外来まとめはチェックなし
           IF     (LNK-ORCSC-PG        =   "KT01" )
                GO      TO      200-ALL-SYORI-EXT
           END-IF
      *!!!
      *
      *    各検診からのダミーがある時、診察料を削除する
           IF    ((FLG-KENSIN          =   1  )  OR
                  (FLG-DUMMY-ARI       =   1  ))  AND
                 ((IDX-ZAI1            >   ZERO) OR
                  (IDX-ZAI2            >   ZERO))
               PERFORM 291-DUMMY-DELETE-SEC
               IF      FLG-DUMMY           =   1
                   GO      TO      200-ALL-SYORI-EXT
               END-IF
           END-IF
      *    初診料・再診料の重複チェック
           IF     (IDX-ZAI1            >   ZERO) AND
                  (IDX-ZAI2            >   ZERO) AND
                  (IDX-SYU2            >   ZERO)
      *        同日初診は再診と同時
               IF      (FLG-DOUJITU        =   1    )  AND
                       (IDX-ZAI1           =   1    )
                   CONTINUE
               ELSE
                   MOVE    "0017"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-SYU2)
                   GO      TO      200-ALL-SYORI-EXT
               END-IF
           END-IF
      *    同日初診
           IF    ((IDX-ZAI1            <=  2   )  OR
                  (IDX-ZAI2            >   ZERO)) AND
                  (FLG-DOUJITU         =   1   )
               CONTINUE
           ELSE
               IF    ((IDX-ZAI1            >   1   ) OR
                      (IDX-ZAI2            >   1   )) AND
                     ( IDX-SYU2            >   ZERO )
                   MOVE    "0017"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR(IDX-SYU2)
                   GO      TO      200-ALL-SYORI-EXT
               END-IF
           END-IF
      *    同日初診チェック
           IF     (FLG-DOUJITU         =   1   )
      *!!!
      *    外来まとめはチェックなし
           AND    (LNK-ORCSC-KT01FLG   =   SPACE  )
      *!!!
               PERFORM 20015-DOUJIT-CHK-SEC
           END-IF
      *    新規患者入力の時、必ず初診料であること
           IF     (LNK-ORCSC-PG        =   "K02"  )  AND
                  (LNK-ORCSC-SYOSIN    =   1      )
      *!!!
      *    外来まとめはチェックなし
           AND    (LNK-ORCSC-KT01FLG   =   SPACE  )
      *!!!
               IF     (IDX-ZAI1            =   ZERO) AND
                      (IDX-ZAI2            =   1   )  AND
                      (IDX-SYU2            >   ZERO )
                   MOVE    "1017"          TO  SPA-ERRCD
                   MOVE    "1058"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-SYU2)
                   GO      TO      200-ALL-SYORI-EXT
               END-IF
           END-IF
      *    他保険算定済みのコメントがある時、診察料がないこと
           IF     (FLG-TAHOKEN         =   1    )  AND
                  (IDX-SYU2            >   ZERO )
               IF      (IDX-ZAI1       >   ZERO )  OR
                       (IDX-ZAI2       >   ZERO )
                   IF     ((IDX-ZAI1       =   1  ) AND
                           (FLG-TAKA       =   1  ))  OR
                          ((IDX-ZAI2       =   1  ) AND
                           (FLG-TAKA       =   1  ))
                       CONTINUE
                   ELSE
                       MOVE    "0110"      TO  SPA-ERRCD
                       MOVE    "1"         TO  WRK-COM-CUR (IDX-SYU2)
                       GO      TO      200-ALL-SYORI-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    初診・再診料の算定チェック
      *    訂正・入院の時、チェックなし
           IF     (LNK-ORCSC-SYORIFLG  =   ZERO )  AND
                  (SPA-NYUGAIKBN       =   "2"  )
            AND   (LNK-ORCSC-PG        =   "K02" )
               IF     (LNK-ORCSC-CHK-IDX   >   ZERO       )  AND
      *???????????    (LNK-ORCSC-LSTYMD    =   SPA-SRYYMD )  AND
      *               同日再診有無
                      (LNK-ORCSC-RRK-LSTYMD    NOT =   SPACE )
      *            労災には同日再診はないのでチェックなし
                   IF    ((SPA-HKNKBN          =   1  )
                     AND  (SPA-RSI-JUNKYO  NOT =   "1" ) )
      *            当日に診察料が算定してないときはチェックなし
                       OR (SPA-DOUJITU         =   ZERO)
      *            当日に初診算定があり、同日初診の時
                     OR ((SPA-SYOYMD          =   LNK-ORCSC-RRK-LSTYMD)
                      AND (IDX-ZAI1           =   1    )  AND
                          (FLG-DOUJITU        =   1    ))
                       CONTINUE
                   ELSE
                       MOVE    "0101"              TO  LNK-ORCSC-KIDCD
      *                初診料の時、算定可能
                       IF     (IDX-ZAI1            =   1  )  AND
                              (WRK-COM-SRYKBN (IDX-SYU2)
                                                   =   "11" )
                           IF      WRK-COM-KZMFLG(IDX-SYU2)  =   SPACE
                               MOVE    "1"         TO  WRK-COM-KZMFLG
                                                           (IDX-SYU2)
                           ELSE
                               MOVE    SPACE       TO  LNK-ORCSC-KIDCD
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      LNK-ORCSC-KIDCD NOT =   SPACE
                GO      TO      200-ALL-SYORI-EXT
           END-IF
      *    以下外来のみとする
           IF      SPA-NYUGAIKBN       =   "1"
            OR    (LNK-ORCSC-PG    NOT =   "K02" )
                GO      TO      200-ALL-SYORI-EXT
           END-IF
      *
           IF     (SPA-HKNKBN          =   1   )
      *D     AND  (SPA-RSI-HKNKBN  NOT =   "5" )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
      *        労災の時、外来管理加算の読み替えチェック
               IF     (IDX-ZAI1            =   ZERO ) AND
                      (LNK-ORCSC-SINSATU-ARI   =   1)
      *            再診の時で再診料のある時
                   PERFORM 20011-RSIGAIKAN-SYORI-SEC
               END-IF
           ELSE
      *        労災以外の外来管理加算の算定チェック
               PERFORM 20012-GAIKAN-SYORI-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (WRK-KEICD       NOT =   SPACE )
               IF      (WRK-COM-KZMFLG (WRK-KEICD-IDX)  =   SPACE)
                 AND   (LNK-ORCSC-KEI-IDX       =   ZERO  )
                   MOVE    WRK-KEICD-IDX   TO  LNK-ORCSC-KEI-IDX
                   MOVE    "1"             TO  WRK-COM-KZMFLG
                                                       (WRK-KEICD-IDX)
                   MOVE    "1"             TO  WRK-COM-CUR
                                                       (WRK-KEICD-IDX)
                   MOVE    WRK-KEICD       TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    労災で救急医療管理加算算定は初診料算定時のみとする
           IF      WRK-RSISYOSIN       >   ZERO
               IF     (IDX-ZAI1        >   ZERO  )  AND
                      (IDX-ZAI2        =   ZERO  )
                   CONTINUE
               ELSE
                   MOVE    "8010"         TO  SPA-ERRCD
                   MOVE    "1"            TO  WRK-COM-CUR(WRK-RSISYOSIN)
                   GO      TO      200-ALL-SYORI-EXT
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        特定薬剤治療管理料の算定チェック
               PERFORM 20010-TOKUTEI-ALLCHK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE ) AND
      *        寝たきり老人退院時共同指導料のチェック
                  (SPA-KAIHOU2-ARI     =   1     )
               IF    ( LNK-ORCSC-SYOSIN    =   1  )  OR
                     ((LNK-ORCSC-SYORIFLG  =   1  )  AND
                      (SPA-SYOYMD1         =   SPA-SRYYMD ))
                   IF      IDX-ZAI1        =   ZERO
                       MOVE    "1043"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (1)
                   END-IF
               END-IF
           END-IF
      *
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    同日初診チェック処理
      *****************************************************************
       20015-DOUJIT-CHK-SEC               SECTION.
      *
      *    同日初診は科が違うこと
           IF      IDX-SYU2        >   ZERO
                IF      WRK-COM-SRYKA(IDX-SYU2)
                                       =   WRK-COM-SRYKA(IDX-SINSATU2)
                   IF      FLG-TAKA        =   ZERO
                       MOVE    "2067"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "2070"          TO  SPA-ERRCD
                   END-IF
                   MOVE    "1"             TO  WRK-COM-CUR
                                                       (IDX-SINSATU2)
               END-IF
           ELSE
      *        当日に他の受診があること（外来のみ）
               IF     ( SPA-SYORIFLG        =   ZERO     ) AND
                      ( SPA-RRK-LSTYMD      =   SPA-SRYYMD)
      *            他科の診療
                   IF    ( SPA-TAKA-SRYKA-R NOT =   SPACE  ) OR
                         ( SPA-SRYKA        NOT =   WRK-COM-SRYKA
                                                     (IDX-SINSATU2))
                       CONTINUE
                   ELSE
                       MOVE    "2067"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                       (IDX-SINSATU2)
                   END-IF
               ELSE
                   IF     (SPA-SYORIFLG        =   ZERO     ) AND
                          (SPA-NYUGAIKBN       =   "2"      )
                       MOVE    "2069"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR
                                                       (IDX-SINSATU2)
                   END-IF
               END-IF
           END-IF
      *    複数科コメントがある時、自動発生分は削除
           IF     (FLG-TAKA-2          =   1 ) AND
                  (SPA-ERRCD           =   SPACE )
               MOVE    ZERO                TO  FLG-TAKA-3
               PERFORM VARYING IDX-Z   FROM    1   BY  1
                       UNTIL  (IDX-Z       >=  IDX-SINSATU2 )   OR
                              (SPA-ERRCD       NOT =   SPACE )
                   IF      WRK-GMN-INPUTCD(IDX-Z)(1:1) =   "$"
                       MOVE    1               TO  FLG-TAKA-3
                   END-IF
      *            同日初診と同じ科
                   IF     (WRK-COM-SRYKA (IDX-Z)
                                           =   WRK-COM-SRYKA
                                                       (IDX-SINSATU2))
                      AND (FLG-TAKA-3      =   1    )
                       IF    ((WRK-COM-INPUTCD(IDX-Z)  =  "830000020")
                         AND  (WRK-COM-SRYKBN (IDX-Z)  =  "11"      ))
                        OR   ((WRK-COM-INPUTCD(IDX-Z)  =  "830000021")
                         AND  (WRK-COM-SRYKBN (IDX-Z)  =  "12"      ))
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                               (IDX-Z)
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                         (IDX-Z - 1)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       20015-DOUJIT-TAKACHK-EXT.
           EXIT.
      *****************************************************************
      *    重複チェック処理
      *****************************************************************
       2000-JYUFUKU-CHK-SEC               SECTION.
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE      )  OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
             IF   (WRK-COM-HKNCOMBI (IDX-Z) =   "9999" )
               CONTINUE
             ELSE
               MOVE    ZERO                TO  FLG-CHK
               MOVE    ZERO                TO  FLG-KEI
      *        診察で手技の時、重複チェックを行う
               IF     (WRK-COM-SRYKBN(IDX-Z)(1:1)  =   "1"   )  AND
                      (WRK-COM-INPUTCD(IDX-Z)(1:1) =   "1"  )   AND
                      (WRK-COM-DATAKBN(IDX-Z)      =   "1"  )
      *            特定薬剤治療管理料は対象外とする
                   IF    WRK-COM-INPUTCD(IDX-Z)      =   "113000410" OR
                                                         "113000510"
                       CONTINUE
                   ELSE
                       MOVE    1                 TO  FLG-CHK
                   END-IF
      *            診療情報提供料（１）は重複時警告とする
                   IF    WRK-COM-INPUTCD (IDX-Z) =   "180016110"
      *??                                        OR  "113009510"
                      MOVE    1                  TO  FLG-KEI
                   END-IF
      *            傷病手当意見書交付料・療養費同意書交付料は対象外
                   IF    WRK-COM-INPUTCD(IDX-Z)      =   "180000710"
                                                     OR  "113004310"
                       MOVE    ZERO              TO  FLG-CHK
                   END-IF
               END-IF
      *
               IF      FLG-CHK             =   1
                   MOVE    ZERO                TO  IDX-KEI
                   COMPUTE IDX-Z3          =   IDX-Z   +   1
                   PERFORM VARYING     IDX-Z2  FROM    IDX-Z3 BY  1
                           UNTIL      (IDX-Z2      >   SPA-MAX-LINE) OR
                                      (SPA-ERRCD   NOT =   SPACE )  OR
                                      (IDX-KEI         >   ZERO  )  OR
                                      (WRK-GMN-INPUTCD (IDX-Z2)
                                                       =   SPACE )
                     IF   (WRK-COM-HKNCOMBI (IDX-Z2) NOT =   "9999" )
                       IF     (WRK-COM-SRYKBN  (IDX-Z)
                                           =   WRK-COM-SRYKBN(IDX-Z2))
                         AND  (WRK-COM-INPUTCD (IDX-Z)
                                           =   WRK-COM-INPUTCD(IDX-Z2))
      *                    警告
                           IF      FLG-KEI         =   1
                               MOVE    IDX-Z2          TO  IDX-KEI
                           ELSE
      *                    エラー
                               MOVE    "0134"          TO  SPA-ERRCD
                               MOVE    "1"             TO  WRK-COM-CUR
                                                              (IDX-Z2)
                           END-IF
                       END-IF
                     END-IF
                   END-PERFORM
      *
      *            警告
                   IF      FLG-KEI         =   1
                       IF     (IDX-KEI         =   ZERO )  AND
                              (WRK-COM-KAISU(IDX-Z)    >   1  )
                           MOVE    IDX-Z           TO  IDX-KEI
                       END-IF
                       IF      IDX-KEI             >   ZERO
                           IF     (WRK-COM-KZMFLG (IDX-KEI)
                                                   =   SPACE )  AND
                                  (LNK-ORCSC-KEI-IDX   =   ZERO )
                               MOVE    IDX-KEI     TO  LNK-ORCSC-KEI-IDX
                               MOVE    "K016"      TO  SPA-ERRCD
                               MOVE    "1"         TO  WRK-COM-CUR
                                                             (IDX-KEI)
                               MOVE    "1"         TO  WRK-COM-KZMFLG
                                                             (IDX-KEI)
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
             END-IF
           END-PERFORM
      *
           .
       2000-JYUFUKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診察料チェック処理
      *****************************************************************
       2001-SINSATU-CHK-SEC                  SECTION.
      *
      *    診察料セット
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-KBN
                       MOVE    SPACE           TO  WRK-KBN2
                       MOVE    ZERO            TO  WRK-KAISU
                       MOVE    ZERO            TO  FLG-TBL
                   WHEN    WRK-COM-INPUTCD(IDX-Z)
                                               =   TBL-SRYCD (TBL-IDX)
                       MOVE    TBL-KAISU (TBL-IDX) TO  WRK-KAISU
                       MOVE    TBL-KBN (TBL-IDX)   TO  WRK-KBN
                       MOVE    TBL-KBN2(TBL-IDX)   TO  WRK-KBN2
                       MOVE    1                   TO  FLG-TBL
           END-SEARCH
      *
      *    労災保険のチェック
           IF     (SPA-HKNKBN          =   1   )
      *****  AND  (SPA-RSI-HKNKBN  NOT =   "5" )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
      *        労災保険の時、電話再診以外はエラーとする
               IF     (WRK-KAISU       =   ZERO    )  AND
                      (WRK-KBN         =   SPACE   )  AND
                      (WRK-KBN2        =   SPACE   )
                   CONTINUE
               ELSE
      *
      *            電話再診もエラー
                   IF      WRK-KBN         =   "T" OR  "S"
                       MOVE    "8021"          TO  WRK-ERRCD
                   ELSE
                       MOVE    "8001"          TO  WRK-ERRCD
                   END-IF
                   MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
      *            外来診察料の判定（２００床以上）はＯＫとする
                   IF     (SPA-BEDSUIPN        >=  200 )  AND
                          (SPA-HOSPSBT         =   1   )
                       IF      WRK-KAISU           =   3
                           MOVE    SPACE           TO  WRK-ERRCD
                           MOVE    SPACE           TO  WRK-COM-CUR
                                                            (IDX-Z)
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    初診料の判定
      **** IF      WRK-KBN         NOT =   SPACE
           IF      FLG-TBL             =   1
               IF      WRK-KBN             =   "F"
      *            同日初診
                   IF      WRK-KBN2            =   "D"
                       MOVE    1               TO  LNK-ORCSC-DOUSIN
                   ELSE
                       MOVE    1               TO  LNK-ORCSC-SYOSIN2
                   END-IF
               ELSE
      *            訂正で初診料からの変更時
                   IF     (LNK-ORCSC-PG        =   "K02") AND
                          (LNK-ORCSC-SYORIFLG  =   1   )  AND
                          (LNK-ORCSC-SYOSIN2   =   1   )  AND
                          (SPA-SYOYMD-OLD      =   SPA-SRYYMD) AND
                          (SPA-SYOYMD1         =   SPACE   OR
                                                   SPA-SYOYMD-OLD)
                       MOVE    "0139"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   ELSE
                       MOVE    ZERO            TO  LNK-ORCSC-SYOSIN2
                   END-IF
               END-IF
           END-IF
      *
      *    外来診察料の判定（２００床以上）
           IF     (SPA-BEDSUIPN        >=  200 )  AND
                  (SPA-HOSPSBT         =   1   )
               IF      WRK-KAISU           =   3
                   CONTINUE
               ELSE
      *            電話再診はエラーとする
                   IF      WRK-KBN         =   "T" OR  "S"
                       MOVE    "8050"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
               END-IF
           END-IF
      *
      *    診察料あり
           IF      WRK-KAISU           >   ZERO
               MOVE    1               TO  LNK-ORCSC-SINSATU-ARI
           END-IF
      *
           .
       2001-SINSATU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    労災診察料の判定処理
      *****************************************************************
       20012-RSISINSATU-CHK-SEC          SECTION.
      *
      *    診察料検索
           SET     TBL-RSI              TO  1
           SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
              AT   END
                   MOVE    SPACE               TO  WRK-KBN
                   MOVE    SPACE               TO  WRK-KBN2
                   MOVE    ZERO                TO  WRK-KAISU
               WHEN    WRK-COM-INPUTCD(IDX-Z)  =   TBLR-SRYCD (TBL-RSI)
                   MOVE    TBLR-KBN  (TBL-RSI) TO  WRK-KBN
                   MOVE    TBLR-KBN2 (TBL-RSI) TO  WRK-KBN2
                   MOVE    TBLR-KAISU(TBL-RSI) TO  WRK-KAISU
           END-SEARCH
      *
           IF      WRK-KBN             =   "F"  OR  "S"
      *        診察料あり
               MOVE    1               TO  LNK-ORCSC-SINSATU-ARI
      *        初診料の判定
               IF      WRK-KBN             =   "F"
      *            同日初診
                   IF      WRK-KBN2            =   "D"
                       CONTINUE
                       MOVE    1               TO  LNK-ORCSC-DOUSIN
                   ELSE
                       MOVE    1               TO  LNK-ORCSC-SYOSIN2
                   END-IF
               ELSE
      *            訂正で初診料からの変更時
                   IF     (LNK-ORCSC-PG        =   "K02") AND
                          (LNK-ORCSC-SYORIFLG  =   1   )  AND
                          (LNK-ORCSC-SYOSIN2   =   1   )  AND
                          (SPA-SYOYMD-OLD      =   SPA-SRYYMD) AND
                          (SPA-SYOYMD1         =   SPACE   OR
                                                   SPA-SYOYMD-OLD)
                       MOVE    "0139"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   ELSE
                       MOVE    ZERO            TO  LNK-ORCSC-SYOSIN2
                   END-IF
               END-IF
      *
               MOVE    WRK-KBN         TO  WRK-SINSATU-KBN
               MOVE    WRK-KAISU       TO  WRK-SINSATU-KAISU
               MOVE    IDX-Z           TO  IDX-SINSATU
      *        同日初診
               IF     (WRK-KBN             =   "F" )  AND
                      (WRK-KBN2            =   "D" )
                   MOVE    1               TO  FLG-DOUJITU
                   MOVE    IDX-Z           TO  IDX-SINSATU2
               END-IF
           END-IF
      *
           .
       20012-RSISINSATU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    労災コードの判定処理
      *****************************************************************
       200112-RSI-KEI-CHK-SEC          SECTION.
      *
      *    労災の救急医療管理加算、再診料療養指導料は対象外とする
           IF       WRK-COM-INPUTCD(IDX-Z) =    "101110030"  OR
                                                "101120030"
                                           OR   "101110020"
               MOVE    1                   TO  FLG-RSIFLG
           ELSE
               MOVE    ZERO                TO  FLG-RSIFLG
           END-IF
      *
      *    労災の救急医療管理加算算定有無
           IF       WRK-COM-INPUTCD(IDX-Z) =   "101110030"
               MOVE    IDX-Z               TO  WRK-RSISYOSIN
           END-IF
      *
      *
           .
       200112-RSI-KEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    外来管理加算コード追加処理
      *****************************************************************
       20011-GAIKASAN-SET-SEC                  SECTION.
      *    労災の外来管理加算
           IF      SPA-HKNKBN          =   1
      *****  AND  (SPA-RSI-HKNKBN  NOT =   "5" )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
               PERFORM 200111-RSI-GAIKASAN-SEC
           ELSE
      *        外来管理加算
               MOVE    TBLG-SRYCD          TO  WRK-SRYCD
           END-IF
      *
      *    一般の時、病院・診療所は同じ
      *    IF      LNK-ORCSC-ROUJIN    =   ZERO
      *        MOVE    ZERO            TO  WRK-HOSPSBT
      *    ELSE
      *        MOVE    SPA-HOSPSBT     TO  WRK-HOSPSBT
      *    END-IF
      *
      *    MOVE    "G"                 TO  WRK-KBN
      *
      *    診察料セット
      *    SET     TBLG-IDX             TO  1
      *    SEARCH  TBL-GAIRAI-OCC       VARYING   TBLG-IDX
      *            AT   END
      *                MOVE    SPACE           TO  WRK-SRYCD
      *            WHEN   (LNK-ORCSC-ROUJIN    =   TBLG-ROUJIN
      *                                                      (TBLG-IDX))
      *              AND  (WRK-HOSPSBT         =   TBLG-HOSPSBT
      *                                                      (TBLG-IDX))
      *              AND  (WRK-KBN             =   TBLG-KBN 
      *                                                      (TBLG-IDX))
      *                MOVE    TBLG-SRYCD (TBLG-IDX)
      *                                        TO  WRK-SRYCD
      **** END-SEARCH
      *
           .
       20011-GAIKASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算コード追加処理（労災）
      *****************************************************************
       200111-RSI-GAIKASAN-SEC            SECTION.
      *
      *    平成１５年９月１日から逓減廃止
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    "G"                 TO  WRK-KBN
      *
      *
      *    診察料検索
           SET     TBL-RSI              TO  1
           SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
              AT   END
                   MOVE    ZERO               TO  FLG-ARI
                   MOVE    SPACE              TO  WRK-SRYCD
               WHEN   (WRK-KAISU       =   TBLR-KAISU (TBL-RSI)) AND
                      (WRK-KBN         =   TBLR-KBN   (TBL-RSI))
                   MOVE    1                  TO  FLG-ARI
                   MOVE    TBLR-SRYCD (TBL-RSI)
                                              TO  WRK-SRYCD
           END-SEARCH
      *
           .
       200111-RSI-GAIKASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算コード追加処理
      *****************************************************************
       2001-GAIKASAN-SEC            SECTION.
      *
      *    外来管理加算コードセット
           MOVE    ZERO                TO  FLG-ARI
           PERFORM 20011-GAIKASAN-SET-SEC
      *
           COMPUTE IDX-S               =   IDX-Z   +  1
      *
           MOVE    ZERO                TO  FLG-ADD
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "1"                 TO  LNK-SC92-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-15MIMAN-ARI   TO  LNK-SC92-15MIMAN-ARI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE           TO  SPA-ERRCD
               INITIALIZE              WRK-GMN-TBLREC (IDX-S)
               INITIALIZE              WRK-COM-TBLREC (IDX-S)
               COMPUTE IDX-S           =   IDX-S
                                       -   1
               GO      TO      2001-GAIKASAN-EXT
           END-IF
      *
           MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYKBN(IDX-S)
           MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
           MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYSYUKBN(IDX-S)
      *
      *    自動追加分（入力時に追加）
      **** MOVE    "1"                 TO  WRK-COM-AUTOKBN (IDX-S)
           MOVE    SPACE               TO  WRK-COM-AUTOKBN (IDX-S)
      *
           MOVE    1                   TO  FLG-ADD
           ADD     1                   TO  WRK-GMN-MAX
      *@@
      *    時間のコメント
      *    MOVE    "840000063"         TO  WRK-SRYCD
      *    PERFORM 1110-KASAN-CHK-SEC
      *    IF      FLG-ARI             =   ZERO
      *        COMPUTE IDX-S               =   IDX-Z   +  1
      *        PERFORM 1100-KASAN-SET-SEC
      *****END-IF
           .
       2001-GAIKASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    指導料初診日チェック処理
      *****************************************************************
       230-SIDOU-SYOSIN-SEC            SECTION.
      *
      *    電話再診の時、エラーとする
           IF      LNK-ORCSC-TELSAI-ARI    =   1
               MOVE    "0077"          TO  SPA-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
               GO      TO      230-SIDOU-SYOSIN-EXT
           END-IF
      *    アフターケアの時、チェックなし
           IF      SPA-RSI-HKNKBN      =   "3"
               GO      TO      230-SIDOU-SYOSIN-EXT
           END-IF
      *
           IF      LNK-ORCSC-SYORIFLG      =   1
      *        訂正時点の初診算定日
               MOVE    LNK-ORCSC-SYOYMD-OLD    TO  WRK-SYOYMD
           ELSE
               MOVE    LNK-ORCSC-SYOYMD        TO  WRK-SYOYMD
               IF     (LNK-ORCSC-SYOYMD-OLD NOT =  SPACE ) AND
                      (WRK-SYOYMD               >  SPA-SRYYMD)
                   MOVE    LNK-ORCSC-SYOYMD-OLD    TO  WRK-SYOYMD
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYOHEN
      *    同じ科で同日初診がある時、同日初診
           IF      WRK-COM-SRYKA (IDX-Z)   =   SPACE
               MOVE    SPA-SRYKA           TO  WRK-SRYKA
           ELSE
               MOVE    WRK-COM-SRYKA (IDX-Z)   TO  WRK-SRYKA
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )   OR
                              (SPA-DOU-SRYKA (IDX) =   SPACE)
               IF      SPA-DOU-SRYKA (IDX) =   WRK-SRYKA
                   IF      SPA-DOU-SRYYMD(IDX) >   WRK-SYOYMD
                       MOVE    SPA-DOU-SRYYMD(IDX)
                                               TO  WRK-SYOYMD
                   END-IF
                   MOVE    10          TO  IDX
               END-IF
           END-PERFORM
      *
      *    最終退院日がある時、退院日
           IF     (LNK-ORCSC-TAIINYMD  =   SPACE )  OR
                  (LNK-ORCSC-TAIINYMD  <=  WRK-SYOYMD)
               MOVE    ZERO                TO  FLG-TAIIN
           ELSE
               MOVE    LNK-ORCSC-TAIINYMD  TO  WRK-SYOYMD
               MOVE    1                   TO  FLG-TAIIN
           END-IF
      *!!!
      *    他院退院日がある時、退院日
           IF      FLG-TAITAIIN        =   1
               MOVE    SPA-SRYYMD          TO  WRK-SYOYMD
               MOVE    2                   TO  FLG-TAIIN
           ELSE
               IF     (SPA-TAI-TAIINYMD    =   SPACE )  OR
                      (SPA-TAI-TAIINYMD    <=  WRK-SYOYMD)
                   CONTINUE
               ELSE
                   MOVE    SPA-TAI-TAIINYMD    TO  WRK-SYOYMD
                   MOVE    2                   TO  FLG-TAIIN
               END-IF
           END-IF
      *    同日初診の入力がある時
           IF     (FLG-DOUJITU         =   1 )  AND
                  (IDX-SINSATU2        >   ZERO)
               IF      WRK-COM-SRYKA(IDX-SINSATU2) =   WRK-SRYKA
                   MOVE    SPA-SRYYMD      TO  WRK-SYOYMD
                   MOVE    1               TO  FLG-SYOHEN
               END-IF
           END-IF
      *    初診入力がある時
           IF      LNK-ORCSC-SYOSIN2   =   1
               MOVE    SPA-SRYYMD      TO  WRK-SYOYMD
               MOVE    1               TO  FLG-SYOHEN
           END-IF
      *@
      *    小児指導管理料は退院日のみ判定とする
           IF      WRK-COM-INPUTCD(IDX-Z)  =   "113002210"
               IF     (FLG-TAIIN           =   1
                                           OR  2    ) OR
                      (WRK-SYOYMD(1:6)     =   SPA-SRYYMD(1:6))
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  WRK-SYOYMD
               END-IF
           END-IF
      *@
      *
           MOVE    ZERO                TO  FLG-KYUJITU
           IF      WRK-SYOYMD          =   SPACE
               MOVE    SPA-SRYYMD          TO  WRK-KEIYMD
               MOVE    SPA-SRYYMD          TO  WRK-KEIYMDZEN
           ELSE
      *        初診料算定日＋１月後
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-SYOYMD          TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    1                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
      *TEST
      *         DISPLAY "C10 WRK-SYOYMD:" WRK-SYOYMD
      *           ",LNK-DAY6-KEISAN:" LNK-DAY6-KEISAN
      *           ",LNK-DAY6-KEISANZEN:" LNK-DAY6-KEISANZEN
      *TEST
               MOVE    LNK-DAY6-KEISAN     TO  WRK-KEIYMD
               MOVE    LNK-DAY6-KEISANZEN  TO  WRK-KEIYMDZEN
               IF      LNK-DAY6-KEISAN NOT =   LNK-DAY6-KEISANZEN
                   MOVE    1                   TO  FLG-KYUJITU
               END-IF
           END-IF
      *
           EVALUATE    TRUE
               WHEN    WRK-KEIYMD          <   SPA-SRYYMD
                   CONTINUE
               WHEN    WRK-KEIYMD          >   SPA-SRYYMD
      *            １月後の前営業日と同じ時、ＯＫ
                   IF      FLG-SIDOU       =   1
                       EVALUATE    FLG-TAIIN
                       WHEN    0
                           MOVE    "0034"         TO  SPA-ERRCD
                       WHEN    1
                           MOVE    "0120"         TO  SPA-ERRCD
                       WHEN    2
                           MOVE    "0121"         TO  SPA-ERRCD
                       END-EVALUATE
                   ELSE
                       IF      WRK-KEIYMDZEN   <=   SPA-SRYYMD
                           IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE)
                             AND  (LNK-ORCSC-KEI-IDX       =   ZERO )
                               MOVE    IDX-Z       TO  LNK-ORCSC-KEI-IDX
                               MOVE    "1"         TO  WRK-COM-KZMFLG
                                                                (IDX-Z)
      **                       IF      FLG-TAIIN       =   1
      *                            MOVE    "K022"          TO  SPA-ERRCD
      *                        ELSE
      *                            MOVE    "K002"          TO  SPA-ERRCD
      ***                      END-IF
                               EVALUATE    FLG-TAIIN
                               WHEN    0
                                   MOVE    "K002"         TO  SPA-ERRCD
                               WHEN    1
                                   MOVE    "K022"         TO  SPA-ERRCD
                               WHEN    2
                                   MOVE    "K032"         TO  SPA-ERRCD
                               END-EVALUATE
                           END-IF
                       ELSE
      *                     IF      FLG-TAIIN       =   1
      *                         MOVE    "0120"          TO  SPA-ERRCD
      *                     ELSE
      *                         MOVE    "0034"          TO  SPA-ERRCD
      *                     END-IF
                           EVALUATE    FLG-TAIIN
                           WHEN    0
                               MOVE    "0034"         TO  SPA-ERRCD
                           WHEN    1
                               MOVE    "0120"         TO  SPA-ERRCD
                           WHEN    2
                               MOVE    "0121"         TO  SPA-ERRCD
                           END-EVALUATE
                       END-IF
                   END-IF
      *!!!
      *       初診算定日からの１ヵ月以内のエラーで、同日初診・退院以外
      *       労災・労災外の初診算定がある時
                   IF     (SPA-ERRCD           =   "0034")  AND
                          (FLG-SYOHEN          =   ZERO  )  AND
                          (SPA-SYOYMD-RSI  NOT =   SPACE )
      *****        AND    (SPA-SYOYMD-HKN  NOT =   SPACE )
                       PERFORM 2302-ERRCHK-SEC
                   END-IF
               WHEN    WRK-KEIYMD          =   SPA-SRYYMD
                   CONTINUE
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
           END-IF
           .
       230-SIDOU-SYOSIN-EXT.
           EXIT.
      *****************************************************************
      *    初診算定日の警告チェック処理
      *****************************************************************
       2302-ERRCHK-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-SYOYMD-K
      *    保険が労災の時
           IF     (SPA-HKNKBN          =   1   )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
      *        規準日が労災以外の初診日であれば、労災の初診日でチェック
               IF     (WRK-SYOYMD          =   SPA-SYOYMD-HKN )
                   MOVE    SPA-SYOYMD-RSI      TO  WRK-SYOYMD-K
               END-IF
           ELSE
               IF     (WRK-SYOYMD          =   SPA-SYOYMD-RSI )
      *        規準日が労災の初診日であれば、労災以外の初診日でチェック
                   MOVE    SPA-SYOYMD-HKN      TO  WRK-SYOYMD-K
      *            労災以外の初診がなければ警告とする
                   IF      SPA-SYOYMD-HKN      =   SPACE
                       MOVE    SPACE           TO  SPA-ERRCD
                       IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE)
                         AND  (LNK-ORCSC-KEI-IDX       =   ZERO )
                           MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                           MOVE    "1"             TO  WRK-COM-KZMFLG
                                                                (IDX-Z)
                           MOVE    "K026"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF      WRK-SYOYMD-K    NOT =   SPACE
      *        初診料算定日＋１月後
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-SYOYMD-K        TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    1                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
      *TEST
      *        DISPLAY "C10 WRK-SYOYMD-K:" WRK-SYOYMD-K
      *          ",LNK-DAY6-KEISAN:" LNK-DAY6-KEISAN
      *          ",LNK-DAY6-KEISANZEN:" LNK-DAY6-KEISANZEN
      *TEST
               IF      LNK-DAY6-KEISAN     <=  SPA-SRYYMD
                   MOVE    SPACE           TO  SPA-ERRCD
               ELSE
                   IF      FLG-SIDOU       NOT =   1
                       IF      WRK-KEIYMDZEN   <=   SPA-SRYYMD
                           MOVE    SPACE           TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   IF     (WRK-COM-KZMFLG (IDX-Z)  =   SPACE)
                     AND  (LNK-ORCSC-KEI-IDX       =   ZERO )
                       MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                                (IDX-Z)
                       MOVE    "K026"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           .
       2302-ERRCHK-EXT.
           EXIT.
      *****************************************************************
      *    入院中の回数チェック処理
      *****************************************************************
       10016-NYUIN-KIKAN-SEC                SECTION.
      *
           INITIALIZE                      ORCSC91AREA
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
      *    IF      FLG-NYUIN           =   2
      *        MOVE    "8"                 TO  LNK-SC91-KBN
      *    ELSE
      *        MOVE    "9"                 TO  LNK-SC91-KBN
      *    END-IF
      *    初回入院日判定なしとする
           MOVE    "9"                 TO  LNK-SC91-KBN
      *    上限回数
           MOVE    WRK-NYUCNT          TO  LNK-SC91-NYUCNT
           MOVE    WRK-COM-INPUTCD (IDX-Z)
                                       TO  LNK-SC91-SRYCD
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC91-TEISEI-AREA
           CALL    "ORCSC91"           USING
                                       MCPAREA
                                       ORCSC91AREA
                                       WRK-SCR-REC
                                       SPA-AREA
                                       TNS-TENSU-REC
      *
           IF      LNK-SC91-ERRCD  NOT =   SPACE
               MOVE    "0212"              TO  SPA-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *   １回のみ
           IF   (WRK-COM-NYU-KAI (IDX-Z)   >   WRK-NYUCNT )  OR
                (LNK-ORCSC-KAISU           >   WRK-NYUCNT )
               MOVE    "0212"              TO  SPA-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
           END-IF
           .
       10016-NYUIN-KIKAN-EXT.
           EXIT.
      *****************************************************************
      *    特定薬剤治療管理料の算定チェック処理
      *****************************************************************
       20010-TOKUTEI-ALLCHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE )  OR
                              (SPA-ERRCD   NOT =   SPACE  )  OR
                              (WRK-GMN-INPUTCD(IDX-Z)  =   SPACE  AND
                               WRK-COM-INPUTCD(IDX-Z)  =   SPACE  )
             IF      WRK-COM-SRYKBN (IDX-Z)  =   "13"
      *
      *        初回算定日をセットする
               EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *            特定薬剤治療管理料
                   WHEN    "113000410"
      *            特定薬剤治療管理料（ジギタリス製剤等）
                   WHEN    "180000110"
                   WHEN    "180000210"
      *            特定薬剤治療管理料（４月目以上）はエラー
                   WHEN    "113000510"
                       IF      LNK-ORCSC-TOKTEI-YMD    =   SPACE
                           MOVE    SPA-SRYYMD  TO  LNK-ORCSC-TOKTEI-YMD
                       END-IF
               END-EVALUATE
      *        特定薬剤治療加算料チェック
               IF      WRK-COM-INPUTCD (IDX-Z)     =   "180000110"  OR
                                                       "180000210"
                   MOVE    ZERO                TO  FLG-SYOKAI2
                   IF      LNK-ORCSC-TOKTEI-YMD    NOT  =  SPACE
      *                算定履歴検索
                       MOVE    WRK-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
                       MOVE    "key3"              TO  WRK-DBPATH
                       PERFORM 100111-SANTEI-KEN-SEC
                       IF      FLG-SANTEI          =   ZERO
      *                    訂正の時、当日分はチェックしない
                           IF     (LNK-ORCSC-SYORIFLG   =   1  )  AND
                                  (SANTEI-SRYYM         =   SPA-SRYYMD
                                                            (1:6))
                               MOVE    SPA-SRYYMD(7:2) TO  IDX-Y2
                               IF      SANTEI-DAY (IDX-Y2) >   ZERO
                                   MOVE    1           TO  FLG-SYOKAI2
                               END-IF
                           END-IF
      *                    今回の初回算定日より前の算定日の時はＯＫ
                           IF     (LNK-ORCSC-TOKTEI-YMD
                                                 >   SANTEI-FSANTEIYMD)
                               OR (FLG-SYOKAI    =   1     )
                               OR (FLG-SYOKAI2   =   1     )
                               CONTINUE
                           ELSE
                               MOVE    "0131"          TO  SPA-ERRCD
                               MOVE    "1"             TO  WRK-COM-CUR
                                                            (IDX-Z)
                           END-IF
                       END-IF
                   END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           .
       20010-TOKUTEI-ALLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算の算定チェック処理
      *****************************************************************
       20012-GAIKAN-SYORI-SEC       SECTION.
      *
      *    外来診療料算定時算定なし
           IF     (SPA-BEDSUIPN        >=  200 )  AND
                  (SPA-HOSPSBT         =   1   )
               GO      TO      20012-GAIKAN-SYORI-EXT
           END-IF
      *
           IF      FLG-GAIKAN          =   1
      *        訂正の時、登録時に判定する
               OR (LNK-ORCSC-TOKUTEI-KNJ2  =   1  )
               OR (LNK-ORCSC-GAIRAI-NASI   =   1  )
               IF      IDX-GAIKAN          >   ZERO
                   MOVE    "0102"              TO  LNK-ORCSC-KIDCD
                   MOVE    IDX-GAIKAN          TO  LNK-ORCSC-CHK-IDX
               END-IF
           ELSE
               IF     (IDX-SYU             >   ZERO )  AND
                      (IDX-GAIKAN          =   ZERO )  AND
                      (LNK-ORCSC-GAIFLG    =   ZERO )  AND
      *************    自動算定なしの時、追加しない
      *               (LNK-ORCSC-GAIKANRIKBN-CHK   NOT =   "2"
      **************                                   AND "3" )
      *                自動算定あり時追加
                      (LNK-ORCSC-GAIKANRIKBN-CHK   =   "0"
                                                   OR  "1" )
                   IF     (LNK-ORCSC-TOKUTEI-KNJ2  =   1  )  OR
                          (LNK-ORCSC-TOKUTEI-KNJ   =   1  )  OR
                          (LNK-ORCSC-GAIRAI-NASI   =   1  )
                       CONTINUE
                   ELSE
                       MOVE    IDX-SYU         TO  IDX-Z
                       PERFORM
                           UNTIL  WRK-COM-DATAKBN (IDX-Z + 1) NOT =  "2"
                               ADD     1               TO  IDX-Z
                       END-PERFORM
      *                外来管理加算コード追加処理
                       PERFORM 2001-GAIKASAN-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       20012-GAIKAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災外来管理加算の読み替え処理
      *****************************************************************
       20011-RSIGAIKAN-SYORI-SEC       SECTION.
      *
      *    外来診療料算定時算定なし
           IF     (SPA-BEDSUIPN        >=  200 )  AND
                  (SPA-HOSPSBT         =   1   )
               GO    TO      20011-RSIGAIKAN-SYORI-EXT
           END-IF
      *
      *    アフターケアの時、外来管理加算を算定しない
           IF      SPA-RSI-HKNKBN      =   "3"
               GO    TO      20011-RSIGAIKAN-SYORI-EXT
           END-IF
      *
      *    外来管理加算がないとき、労災のコードで点数マスタを検索
           IF      LNK-ORCSC-GAIRAI-TEN    =   ZERO
               PERFORM 200111-RSI-GAIKASAN-SEC
               MOVE    WRK-SRYCD           TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   MOVE    TNS-TEN         TO  LNK-ORCSC-GAIRAI-TEN
               END-IF
           END-IF
      *
           MOVE   ZERO                 TO  WRK-GAI-CNT
           MOVE   ZERO                 TO  WRK-GAI-IDX
           MOVE   99999999             TO  WRK-GAI-TEN
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE      )  OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
                MOVE    ZERO                TO  WRK-COM-RSIGIRI
                                                            (IDX-Z)
      *
                IF     (WRK-COM-SRYKBN     (IDX-Z)
                                           NOT =   "95" AND "96") AND
      *!
      *            診療種別が対象外
                       (WRK-COM-SRYSYUKBN (IDX-Z)  NOT =   "409" )  AND
      *
                       (WRK-COM-INPUTCD(IDX-Z)(1:3)
                                           NOT =   "101"  )   AND
                       (WRK-COM-GAIKANRIKBN(IDX-Z)
                                               =   1   )
      *             告示区分２が加算以外
                   AND (WRK-COM-DATAKBN (IDX-Z)    =   "1" )
                   AND (WRK-COM-KOKUJISKBKBN2(IDX-Z)
                                               =   1  OR  3  OR  5)
                   IF     (WRK-COM-SYUTEN2 (IDX-Z) >  ZERO )  AND
                          (WRK-COM-SYUTEN2 (IDX-Z)
                                               <   LNK-ORCSC-GAIRAI-TEN)
                       ADD     1               TO  WRK-GAI-CNT
                       IF      WRK-COM-SYUTEN2 (IDX-Z)
                                               <   WRK-GAI-TEN
                           MOVE    WRK-COM-SYUTEN2 (IDX-Z)
                                               TO  WRK-GAI-TEN
                           MOVE    IDX-Z       TO  WRK-GAI-IDX
                       END-IF
                   END-IF
                END-IF
           END-PERFORM
      *    最初の読み替え分の回数が１以上の時、エラー
           IF      WRK-GAI-CNT     >   ZERO
               IF      WRK-COM-KAISU (WRK-GAI-IDX)   >   1
                   MOVE    "0140"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (WRK-GAI-IDX)
               END-IF
           END-IF
      *
           IF      WRK-GAI-CNT         >   1
      *        外来管理加算へ読み替える
               MOVE    ZERO                TO  WRK-GAI-CNT
               PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE      )  OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
                    IF     (WRK-COM-SRYKBN     (IDX-Z)
                                          NOT =   "95" AND "96") AND
                           (WRK-COM-INPUTCD(IDX-Z)(1:3)
                                           NOT =   "101"  )   AND
                           (WRK-COM-GAIKANRIKBN(IDX-Z)
                                               =   1   )
      *                告示区分２が加算以外
                       AND (WRK-COM-DATAKBN (IDX-Z)    =   "1" )
                       AND (WRK-COM-KOKUJISKBKBN2(IDX-Z)
                                               =   1  OR  3  OR  5)
                       IF     (WRK-COM-SYUTEN2 (IDX-Z)
                                               >   ZERO   )  AND
                              (WRK-COM-SYUTEN2 (IDX-Z)
                                               <   LNK-ORCSC-GAIRAI-TEN)
                           ADD     1               TO  WRK-GAI-CNT
                           IF      IDX-Z       NOT =   WRK-GAI-IDX
                               MOVE    1           TO  WRK-COM-RSIGIRI
                                                            (IDX-Z)
      *                        労災の時、読み替えあり
                               MOVE    "1"         TO  LNK-ORCSC-RSIOK
                           END-IF
                       END-IF
                    END-IF
               END-PERFORM
           END-IF
      *
      *    外来管理加算不可のコードがあり、５２点以下の手技が存在しない
           IF     (FLG-GAIKAN              =   1   OR
                   LNK-ORCSC-TOKUTEI-KNJ2  =   1  )  AND
                  (WRK-GAI-CNT             =   ZERO)
      *        外来管理加算削除
               IF      IDX-GAIKAN          >   ZERO
                   MOVE    "0102"              TO  LNK-ORCSC-KIDCD
                   MOVE    IDX-GAIKAN          TO  LNK-ORCSC-CHK-IDX
               END-IF
           ELSE
               IF     (IDX-GAIKAN          =   ZERO )  AND
                      (LNK-ORCSC-GAIFLG    =   ZERO )  AND
                      (WRK-GAI-CNT         >   ZERO)
                   IF      IDX-SYU2        =   ZERO
                       MOVE    ZERO            TO  IDX-Z
                   ELSE
                       MOVE    IDX-SYU2        TO  IDX-Z
                       PERFORM
                           UNTIL  (WRK-COM-SRYKBN (IDX-Z) NOT =  "12")
                            OR    (WRK-COM-ZAIEND (IDX-Z)     =  "1" )
                           ADD     1               TO  IDX-Z
                       END-PERFORM
      *                外来管理加算コード追加処理
                       PERFORM 2001-GAIKASAN-SEC
      *                労災読み替え加算分
                       IF      FLG-ADD         =   1
                           MOVE    "1"             TO  WRK-COM-RSIFLG2
                                                               (IDX-S)
                       END-IF
                   END-IF
               ELSE
      *            外来管理加算が算定できる時
                   IF     (IDX-SYU2            >   ZERO )  AND
                          (FLG-GAIKAN          =   ZERO )  AND
                          (IDX-GAIKAN          =   ZERO )  AND
                          (LNK-ORCSC-SYOSIN    =   ZERO )  AND
                          (LNK-ORCSC-SYOSIN2   =   ZERO )
      **********       自動算定なしの時、追加しない
      *          AND  (LNK-ORCSC-GAIKANRIKBN-CHK   NOT =   "2" 
      *********                                        AND "3" )
      *                自動算定あり時追加
                 AND  (LNK-ORCSC-GAIKANRIKBN-CHK   =   "0"
                                                   OR  "1" )
                       MOVE    IDX-SYU2        TO  IDX-Z
                       PERFORM
                           UNTIL  (WRK-COM-SRYKBN (IDX-Z)
                                                         NOT =  "12")
                            OR    (WRK-COM-ZAIEND (IDX-Z)    =  "1")
                           ADD     1               TO  IDX-Z
                       END-PERFORM
      *                外来管理加算コード追加処理
                       PERFORM 2001-GAIKASAN-SEC
                   END-IF
      *            外来管理加算の削除
      *            診察料が存在せず、外来管理加算がある時
                   IF     (IDX-SYU2            =   ZERO )  AND
                          (FLG-GAIKAN          =   ZERO )  AND
                          (IDX-GAIKAN          >   ZERO )  AND
                          (WRK-GAI-CNT         =   ZERO )  AND
                          (LNK-ORCSC-SYOSIN    =   ZERO )  AND
                          (LNK-ORCSC-SYOSIN2   =   ZERO )
                       MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                        (IDX-GAIKAN)
                   END-IF
               END-IF
           END-IF
           .
       20011-RSIGAIKAN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    注加算コードのチェック処理
      *****************************************************************
       290-CHUKSNCHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  IDX-CHU
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE   )  OR
                              (WRK-GMN-INPUTCD(IDX)    =   SPACE )
               IF      IDX             NOT =   IDX-Z
      *                    注加算コード、通番が同じ時エラー
                   IF      (WRK-COM-SRYKBN (IDX)
                                           =   WRK-COM-SRYKBN
                                                           (IDX-Z)) AND
                           (WRK-COM-CHUKSNCD   (IDX)
                                           =   WRK-COM-CHUKSNCD
                                                           (IDX-Z)) AND
                           (WRK-COM-CHUKSNTUBAN(IDX)
                                           =   WRK-COM-CHUKSNTUBAN
                                                             (IDX-Z))
                       MOVE    "0042"          TO  SPA-ERRCD
                       MOVE    IDX             TO  IDX-CHU
      *                同じコードで自動発生の場合はＯＫとする
                       IF     (WRK-COM-AUTOKBN (IDX)   =   "2" ) AND
                              (WRK-COM-INPUTCD (IDX)   =
                                               WRK-COM-INPUTCD(IDX-Z))
                           MOVE    SPACE       TO  SPA-ERRCD
                           MOVE    ZERO        TO  IDX-CHU
                       END-IF
                   END-IF
               END-IF
      *        診察料以外は、剤内のチェックとする
               IF      WRK-COM-ZAIEND (IDX)    =   "1"
                   IF      WRK-COM-SRYKBN (IDX-Z)  =   "11"  OR  "12"
                       CONTINUE
                   ELSE
                       IF      IDX                 <   IDX-Z
                           MOVE    SPACE           TO  SPA-ERRCD
                       END-IF
                       IF      IDX                 >=  IDX-Z
                           MOVE    SPA-MAX-LINE        TO  IDX
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           IF      (SPA-ERRCD      NOT =   SPACE )  AND
                   (IDX-CHU        NOT =   ZERO  )
                MOVE    "1"             TO  WRK-COM-CUR(IDX-CHU)
           END-IF
      *
           .
       290-CHUKSNCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ダミー診察料削除処理
      *****************************************************************
       291-DUMMY-DELETE-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-DUMMY
           MOVE    ZERO                TO  IDX-MAE-IDX
           MOVE    ZERO                TO  IDX-DUMMY
      *    診察料
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD(IDX)    =   SPACE AND
                               WRK-COM-INPUTCD(IDX)    =   SPACE )
      *        診察料のチェック
               IF     (WRK-GMN-INPUTCD (IDX)   NOT =   SPACE )  AND
                      (WRK-COM-SRYKBN  (IDX)       =   "11" OR
                                                       "12"  )  AND
      *        労災の振替えは削除しない
                      (WRK-COM-RSIFLG2 (IDX)       =   SPACE )
                   PERFORM 2911-SINSATU-CHK-SYORI-SEC
                   IF      FLG-ARI             =   1
      *                剤を削除する
                       MOVE    1                   TO  FLG-DUMMY
                       PERFORM 2912-ZAIDEL-SYORI-SEC
                   END-IF
               END-IF
      *        ＤＵＭＭＹセット
               IF     (IDX-DUMMY               =   ZERO )  AND
                      (IDX                     >   1    )  AND
                      (WRK-COM-ZAIEND(IDX - 1) =   "1"  ) AND
                      (WRK-COM-RSIFLG2 (IDX - 1 )
                                           NOT =   "1"   )
                   MOVE    IDX                 TO  IDX-MAE-IDX
               END-IF
               IF      WRK-COM-INPUTCD(IDX)    =   "099110001" OR
                                                   "099120001"
                   MOVE    IDX                 TO  IDX-DUMMY
               END-IF
           END-PERFORM
      *
           IF     (IDX-MAE-IDX         =   ZERO )  OR
                  (WRK-COM-RSIFLG2 (IDX-MAE-IDX)
                                       =   "1"   )
               GO      TO      291-DUMMY-DELETE-EXT
           END-IF
      *
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
           INITIALIZE                      WRK-SCR-REC
           INITIALIZE                      HZN2-SCR-REC
           MOVE    ZERO                TO  FLG-DELETE
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-MAX-LINE
               IF     (HZN-GMN-INPUTCD(IDX)    =   SPACE ) AND
                      (HZN-COM-INPUTCD(IDX)    =   SPACE )
                   CONTINUE
               ELSE
                   IF     (IDX-MAE-IDX     <=  IDX       )  AND
                          (FLG-DELETE      =   ZERO      )
                       IF      HZN-COM-ZAIEND (IDX)    =   "1"
                           MOVE    1                   TO  FLG-DELETE
                       END-IF
                   ELSE
                       ADD     1                   TO  IDX2
                       MOVE    HZN-GMN-TBL (IDX)   TO  HZN2-GMN-TBL
                                                                  (IDX2)
                       MOVE    SPACE               TO  HZN-GMN-TBL (IDX)
                   END-IF
               END-IF
           END-PERFORM
      *    ＤＵＭＭＹ診察料を最初へ
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-MAX-LINE
               IF     (HZN-GMN-INPUTCD(IDX)    =   SPACE ) AND
                      (HZN-COM-INPUTCD(IDX)    =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDX2
                   MOVE    HZN-GMN-TBL (IDX)   TO  WRK-GMN-TBL (IDX2)
               END-IF
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-MAX-LINE
               IF     (HZN2-GMN-INPUTCD(IDX)   =   SPACE ) AND
                      (HZN2-COM-INPUTCD(IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDX2
                   MOVE    HZN2-GMN-TBL (IDX)  TO  WRK-GMN-TBL (IDX2)
               END-IF
           END-PERFORM
      *
           .
       291-DUMMY-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料削除処理
      *****************************************************************
       2912-ZAIDEL-SYORI-SEC                SECTION.
      *
           PERFORM VARYING     IDY     FROM    IDX BY  1
                   UNTIL      (IDY         >   SPA-MAX-LINE      )   OR
                              (WRK-GMN-INPUTCD(IDY)    =   SPACE  AND
                               WRK-COM-INPUTCD(IDY)    =   SPACE  )
               IF      WRK-COM-INPUTCD (IDY)   NOT =   "099110001"  AND
                                                       "099120001"
                   MOVE    SPACE               TO  WRK-GMN-INPUTCD(IDY)
               END-IF
               IF      WRK-COM-ZAIEND (IDY)    =   "1"
                   MOVE    SPA-MAX-LINE        TO  IDY
               END-IF
           END-PERFORM
           .
       2912-ZAIDEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診察料チェッ処理
      *****************************************************************
       2911-SINSATU-CHK-SYORI-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-ARI
      *    診察料セット
           SET     TBL-IDX             TO  1
           SEARCH  TBL-SINSATU-OCC     VARYING   TBL-IDX
                   AT   END
                       MOVE    ZERO            TO  FLG-ARI
                   WHEN    WRK-COM-INPUTCD(IDX)
                                               =   TBL-SRYCD (TBL-IDX)
                       IF     (TBL-KBN (TBL-IDX)   =   "F"  )
                       AND    (TBL-KBN2(TBL-IDX)   =   "D"  )
      *                   同日初診は削除なし
                           MOVE    ZERO            TO  FLG-ARI
                       ELSE
                           MOVE    1               TO  FLG-ARI
                       END-IF
           END-SEARCH
      *
           IF      FLG-ARI             =   1
               GO      TO      2911-SINSATU-CHK-SYORI-EXT
           END-IF
      *
      *    外来管理加算判定
           IF      WRK-COM-INPUTCD(IDX)    =   TBLG-SRYCD
               MOVE    1                   TO  FLG-ARI
           END-IF
      **** SET     TBLG-IDX            TO  1
      *    SEARCH  TBL-GAIRAI-OCC      VARYING   TBLG-IDX
      *            AT   END
      *                MOVE    ZERO            TO  FLG-ARI
      *            WHEN    WRK-COM-INPUTCD(IDX)    =   TBLG-SRYCD
      *                                                      (TBLG-IDX)
      *                MOVE    1               TO  FLG-ARI
      **** END-SEARCH
           IF      FLG-ARI             =   1
               GO      TO      2911-SINSATU-CHK-SYORI-EXT
           END-IF
      *
      *    労災保険の外来管理加算の判定
           SET     TBL-RSI             TO  1
           SEARCH  TBL-RSI-SINSATU-OCC  VARYING   TBL-RSI
              AT   END
                   MOVE    ZERO                TO  FLG-ARI
               WHEN    WRK-COM-INPUTCD(IDX)    =   TBLR-SRYCD (TBL-RSI)
                   MOVE    1                   TO  FLG-ARI
           END-SEARCH
           .
       2911-SINSATU-CHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定処理
      *****************************************************************
       300-AUTO-SYORI-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-KANWA
           MOVE    ZERO                TO  FLG-MAYAKU
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY         >   SPA-MAX-LINE      )   OR
                             ((WRK-GMN-INPUTCD(IDY)    =   SPACE ) AND
                              (WRK-COM-INPUTCD(IDY)    =   SPACE ))
      *        がん性疼痛緩和指導管理料
               IF      WRK-COM-INPUTCD (IDY)   =   "113010210"
                   MOVE    IDY             TO  IDX-Z
                   MOVE    1               TO  FLG-KANWA
               END-IF
           END-PERFORM
           IF      FLG-KANWA           =   1
               IF      SPA-NYUGAIKBN       =   1
                   MOVE    ZERO                TO  WRK-DD
                   PERFORM VARYING IDX-N2      FROM    1   BY  1
                           UNTIL  (IDX-N2      >   31  )   OR
                                  (WRK-DD      NOT =   ZERO  )
                       IF      WRK-COM-NYUINDAY(IDX-Z  IDX-N2)
                                                        >   ZERO
                           MOVE    IDX-N2          TO  WRK-DD
                           MOVE    31              TO  IDX-N2
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY         >   SPA-MAX-LINE      )   OR
                             ((WRK-GMN-INPUTCD(IDY)    =   SPACE ) AND
                              (WRK-COM-INPUTCD(IDY)    =   SPACE ))  OR
                              (FLG-KANWA       =   ZERO   )  OR
                              (FLG-MAYAKU      =   1      )
      *        麻薬
               IF     (WRK-COM-INPUTCD (IDY)(1:1)  =   "6")
                  AND (WRK-COM-MADOKUKBN (IDY)     =   1  )
                   IF      SPA-NYUGAIKBN       =   1
                       IF      WRK-COM-NYUINDAY(IDY WRK-DD)
                                                    >   ZERO
                           MOVE    1               TO  FLG-MAYAKU
                       END-IF
                   ELSE
                       MOVE    1               TO  FLG-MAYAKU
                   END-IF
               END-IF
           END-PERFORM
           IF     (FLG-KANWA           =   1    )
              AND (FLG-MAYAKU          =   ZERO )
      *        麻薬がない時、警告とする
               IF     (WRK-COM-KZMFLG (IDX-Z)  NOT =   "2" )
                  AND (LNK-ORCSC-KEI-IDX   =   ZERO  )
                   MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
                   MOVE    "K243"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   MOVE    "2"             TO  WRK-COM-KZMFLG
                                                           (IDX-Z)
               END-IF
           END-IF
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
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
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
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
       990-DBCLOSE-SEC         SECTION.
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
