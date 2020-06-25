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
       PROGRAM-ID.                 ORCSC60200804.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（検査）
      *                      Ｈ２０年４月改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC−多々納  01.04.18  更新前チェックのバグ対応
      *  01.00.02    MCC−多々納  01.05.29  血液学的検査判断料のバグ対応
      *  01.00.03    MCC−多々納  01.06.26  乳幼児加算の自動算定追加
      *  01.00.04    MCC−多々納  02.03.18  施設不適合の追加
      *  01.00.05    MCC-多々納   02/04/16  労災用追加
      *  01.00.06    NACL-多々納  02/10/25  入院対応
      *  01.00.07    NACL-多々納  02/12/03  包括数修正
      *  01.00.08    NACL-多々納  03/01/24  通則年齢から年齢加算を追加
      *  01.00.09    NACL-多々納  03/07/11  検体検査管理加算を追加
      *  01.00.10    NACL-多々納  03/09/11  外来逓減検査同時入力追加
      *                                     外来包括なし（610)追加
      *  01.00.11    NACL-多々納  04/03/15  改正対応
      *  01.00.12    NACL-多々納  04/06/XX  入院初回算定日重複算定チェック追加他
      *                                     複数保険・科追加
      *  01.00.13    NACL-多々納  04/12/09  重複チェック修正
      *  01.00.14    NACL-多々納  05/12/08  MONFUNC 対応 他
      *  01.00.15    NACL-多々納  06/03/XX  Ｈ１８．４改正対応
      *  03.04.00    NACL-多々納  07/01/26  コンタクトレンズ検査チェック追加
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.02.00    NACL-多々納  08/03/XX  Ｈ２０．４改正対応
      *  04.03.00    NACL-多々納  08/07/XX  病理診断加算・検査加算種別追加
      *  04.03.00    NACL-多々納  08/07/25  入院時初回加算自動算定追加
      *  04.04.00    NACL-多々納  09/01/09  前回検査日自動算定追加
      *  04.05.00    NACL-多々納  09/08/20  蛋白分画、総蛋白、アルブミンの算定チェック追加
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
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-SEIBETU         PIC 9(01).
           03  FLG-JIKASAN         PIC 9(01).
      *
           03  FLG-ADD             PIC 9(01).
      *
      *    静脈区分
           03  FLG-BV              PIC 9(01).
      *    動脈区分
           03  FLG-BA              PIC 9(01).
      *    抹消区分 
           03  FLG-BC              PIC 9(01).
      *    採血料算定不可
           03  FLG-FUKA            PIC 9(01).
      *    施設基準不適合
           03  FLG-KIJUNN          PIC 9(01).
      *
           03  FLG-KASAN           PIC 9(01).
           03  FLG-KSN-END         PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
           03  FLG-SYOKAI-OK       PIC 9(01).
      *
           03  FLG-HOUKATU         PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
      *
           03  FLG-NYUDAY           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-Z2              PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-SA              PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
           03  IDX-ACT             PIC 9(04).
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-Y3              PIC 9(04).
      *----(01.00.01) LINE ADD START ----------------------------------
           03  IDX-CHK             PIC 9(04).
      *----(01.00.01) LINE ADD END   ----------------------------------
           03  IDX-JIKAN           PIC 9(04).
           03  IDX-NYU             PIC 9(04).
      *
           03  IDX-TEIGEN          PIC 9(04).
           03  IDX-TEIGEN2         PIC 9(04).
      *
           03  IDT                 PIC 9(04).
           03  IDK                 PIC 9(04).
      *
           03  IDXS-KENTAI         PIC 9(04).
           03  IDX-BV              PIC 9(04).
           03  IDX-BA              PIC 9(04).
           03  IDX-BC              PIC 9(04).
      *
           03  IDX-STR             PIC 9(04).
           03  IDX-SANSO           PIC 9(04).
           03  IDX-KEN             PIC 9(04).
           03  IDX-G               PIC 9(04).
      *
           03  IDX-N2              PIC 9(02).
      *
           03  IDX-SN              PIC 9(04).
           03  IDD                 PIC 9(02).
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-MOJI            PIC X(01). 
           03  WRK-ERRCD           PIC X(04). 
           03  WRK-SRYCD           PIC X(09). 
           03  WRK-SRYKA           PIC X(02). 
           03  WRK-HKNCOMBI        PIC X(04). 
           03  WRK-CHIKENKBN       PIC X(01). 
      *
           03  WRK-IDX             PIC 9(04).
      *    乳幼児の自動加算コード
           03  WRK-NYU-SRYCD       PIC X(09).
      *
           03  WRK-SYOKEIS         PIC 9(08)V99.
           03  WRK-SYOKEIS2        PIC 9(08)V99.
           03  WRK-SYOKEI          PIC 9(08).
      *
           03  WRK-ZAIKEI          PIC 9(08)V99.
           03  WRK-GAIKEI          PIC 9(08)V99.
           03  WRK-ZAITEN          PIC 9(08).
           03  WRK-GAITEN          PIC 9(08).
           03  WRK-KIZKEI          PIC 9(08).
           03  WRK-KIZTEN          PIC 9(08).
           03  WRK-FIRTEN          PIC 9(08).
           03  WRK-MAISU           PIC 9(08).
           03  WRK-KEIMAI          PIC 9(08).
           03  WRK-TENSUKEI        PIC 9(08)V99.
           03  WRK-TENSUKEI-9      PIC 9(08).
           03  WRK-TENSUKEI-G      PIC 9(08).
           03  WRK-TENSUKEI-K      PIC 9(08).
           03  WRK-KAISU           PIC 9(04).
           03  WRK-TENSUKEI-G1     PIC 9(08).
           03  WRK-TENSUKEI-G2     PIC 9(08).
           03  WRK-TENSUKEI-K1     PIC 9(08).
           03  WRK-TENSUKEI-K2     PIC 9(08).
      *
           03  WRK-TENSU           PIC S9(08).
      *    集計
           03  WRK-SYUKEI-AREA.
               05  WRK-TENSUKEI-TEI    PIC 9(08).
               05  WRK-TENSUKEI-NASI   PIC 9(08).
               05  WRK-TENSUKEI-TNEN   PIC 9(08).
               05  WRK-TENSUKEI-NNEN   PIC 9(08).
      *
      *    年齢判定用
           03  WRK-KGNAGE          PIC 9(03).
           03  WRK-JGNAGE          PIC 9(03).
      *
           03  WRK-KEN-HOU         PIC X(02).
      *
           03  WRK-SANTEI-CNT      PIC 9(04).
           03  WRK-TODAY-CNT       PIC 9(03).
           03  WRK-TODAY-CNTG      PIC 9(03).
      *
      *    包括名称編集用
           03  WRK-Z9              PIC Z9.
           03  WRK-HEN-Z9          PIC X(04).
           03  WRK-MEISYO          PIC X(62).
      *
      *    検体検査管理加算
           03  FLG-KENTAI              PIC 9(01).
           03  WRK-KENTAI-DAY          PIC 9(02).
      *    外来迅速検体検査加算
           03  FLG-KENTAI-CHK          PIC 9(01).
           03  FLG-KENTAI2             PIC 9(01).
           03  WRK-KENTAI-SRYKA        PIC X(02). 
           03  WRK-KENTAI-HKNCOMBI     PIC X(04). 
           03  WRK-KENTAI-CHIKENKBN    PIC X(01). 
           03  WRK-KENTAI2-CNT         PIC 9(03). 
           03  FLG-KENTAI-NO            PIC 9(01).
           03  FLG-KENTAI-CHK2          PIC 9(01).
      *
           03  WRK-TEI-CNT             PIC 9(03).
      *
           03  WRK-YMD.
               05  WRK-YM.
                   07  WRK-YY              PIC 9(04).
                   07  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      *
           03  WRK-TNS-JGNAGE      PIC 9(02).
      *
           03  WRK-AUTOKBN         PIC X(01).
      *
           03  WRK-ERRMSG-1        PIC X(80).
           03  WRK-ERRMSG-2        PIC X(80).
      *
      *    検査　判断グループ
       01  WRK-KENSA-AREA.
           03  WRK-KENSA-TBL                       OCCURS   20.
               05  WRK-KEN-SRYKA       PIC X(02).
               05  WRK-KEN-HKNCOMBI    PIC X(04).
               05  WRK-KEN-CHIKENKBN   PIC X(01).
               05  WRK-KENSAGRP        PIC 9(02).
               05  WRK-KEN-KBN         PIC 9(01).
               05  WRK-KEN-SRYCD       PIC X(09).
               05  WRK-KEN-TENSU       PIC 9(05)V9(03).
      *        入院時の初日
               05  WRK-KEN-DAY         PIC 9(02).
           03  WRK-KENIDX              PIC 9(04).
      *
      *    包括対象検査
       01  WRK-HOUKNS-AREA.
           03  WRK-HOUKNS-TBL          OCCURS   11.
               05  WRK-HOUKNS-CNT      PIC 9(04).
               05  WRK-HOUKNS-TENSU    PIC 9(07)V9(03).
      *
      *    入院日
       01  WRK-NYUINDAY-AREA.
           03  WRK-BA-DAY              PIC 9(02).
           03  WRK-BC-DAY              PIC 9(02).
           03  WRK-BV-DAY              PIC 9(02).
           03  WRK-SAI-DAY             PIC 9(02).
           03  IDN1                    PIC 9(02).
           03  IDN2                    PIC 9(02).
           03  FLG-BA-ARI              PIC 9(01).
           03  FLG-BC-ARI              PIC 9(01).
           03  FLG-BV-ARI              PIC 9(01).
      *    入院の回数（Ｂ−Ａ（動脈採血料））
      *    入院日
           03  TBL-NYUINDAY-AREA.
               05  TBL-NYUINDAY-OCC        OCCURS   31.
                   07  TBL-NYUINDAY        PIC 9(01).
           03  WRK-NISUU-AREA.
               05  WRK-NISUU-TBL           OCCURS   31.
                   07  WRK-DAY             PIC 9(02).
                   07  WRK-SEL             PIC X(01).
           03  IDN3                    PIC 9(04).
           03  IDN4                    PIC 9(04).
           03  FLG-DAY                 PIC 9(01).
           03  WRK-H-DAY               PIC 9(02).
           03  WRK-H-SEL               PIC X(01).
           03  WRK-KAISU-H             PIC X(04).
      *    末日
           03  WRK-MATU                    PIC 9(02).
      *    入院時初回加算用
           03  WRK-HOU-DAY             PIC 9(02).
           03  FLG-HOUKNS-ARI          PIC 9(01).
           03  FLG-SYOKAI-ARI          PIC 9(01).
           03  FLG-KENSA-ARI           PIC 9(01).
           03  WRK-HOU-YMD             PIC X(08).
      *
      *    入院逓減用
       01  TBL-TEIGEN-AREA.
           03  TBL-TEIGEN-OCC          OCCURS   20.
               05  TBL-TEI-SRYCD       PIC X(09).
               05  TBL-TEI-HOU         PIC X(02).
               05  TBL-TEI-CNT         PIC 9(03).
       01  IDX-TEI                     PIC 9(04).
      *
      *    保存用ＳＰＡ
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *
      *時間外コメント（日時分）
       01  CONS-JIKAN                  PIC X(09)   VALUE   "840000113".
      *
      *****************************************************************
      *    逓減検査包括テーブル
      *****************************************************************
      *
       01  TBL-KENSA-HOU-AREA.
      *      心カテ（右心）
           03  FILLER          PIC X(12)  VALUE    "01 160064610".
      *      肺臓カテーテル法
           03  FILLER          PIC X(12)  VALUE    "01 160065850".
      *      肺臓カテーテル法
           03  FILLER          PIC X(12)  VALUE    "01 160065850".
      *      肝臓カテーテル法
           03  FILLER          PIC X(12)  VALUE    "01 160065950".
      *      膵臓カテーテル法
           03  FILLER          PIC X(12)  VALUE    "01 160166950".
      * 02
      *    ＥＣＧ１２
           03  FILLER          PIC X(12)  VALUE    "02 160068410".
      *    ＥＣＧ（６誘導以上）
           03  FILLER          PIC X(12)  VALUE    "02 160068510".
      *    体表ヒス束心電図
           03  FILLER          PIC X(12)  VALUE    "02 160068750".
      *    ＶＤＧ
           03  FILLER          PIC X(12)  VALUE    "02 160068850".
      *    バリストカルジオグラフ（１方向）
           03  FILLER          PIC X(12)  VALUE    "02 160069050".
      *    バリストカルジオグラフ（２方向以上）
           03  FILLER          PIC X(12)  VALUE    "02 160069130".
      *    ＥＣＧ１２
           03  FILLER          PIC X(12)  VALUE    "02 160704210".
      *    ＥＣＧ（６誘導以上）
           03  FILLER          PIC X(12)  VALUE    "02 160704310".
      *    ＶＤＧ
           03  FILLER          PIC X(12)  VALUE    "02 160708710".
      *    体表ヒス束心電図
           03  FILLER          PIC X(12)  VALUE    "02 160708810".
      *    携帯型発作時心電図記憶伝送装置使用心電図
           03  FILLER          PIC X(12)  VALUE    "02 160150650".
      * 03
      *    ＥＣＧ負荷１２
           03  FILLER          PIC X(12)  VALUE    "03 160069210".
      *    ＥＣＧ負荷（６誘導以上）
           03  FILLER          PIC X(12)  VALUE    "03 160069310".
      * 04
      *    ホルター型心電図
           03  FILLER          PIC X(12)  VALUE    "04 160069510".
      *    ホルター型心電図（８時間以上）
           03  FILLER          PIC X(12)  VALUE    "04 160069610".
      *    体表面心電図
           03  FILLER          PIC X(12)  VALUE    "04 160069750".
      *    心外膜マッピング
           03  FILLER          PIC X(12)  VALUE    "04 160069850".
      *    ホルター型心電図(３０分まで）
           03  FILLER          PIC X(12)  VALUE    "04 160128430".
      *    ホルター型心電図(３０分超１時間）
           03  FILLER          PIC X(12)  VALUE    "04 160128530".
      *    ホルター型心電図(１時間超１時間３０分）
           03  FILLER          PIC X(12)  VALUE    "04 160128630".
      *    ホルター型心電図(１時間３０分超２時間）
           03  FILLER          PIC X(12)  VALUE    "04 160128730".
      *    ホルター型心電図(２時間超１時間３０分）
           03  FILLER          PIC X(12)  VALUE    "04 160128830".
      *    ホルター型心電図(２時間３０分超３時間）
           03  FILLER          PIC X(12)  VALUE    "04 160128930".
      *    ホルター型心電図(３時間超３時間３０分）
           03  FILLER          PIC X(12)  VALUE    "04 160129030".
      *    ホルター型心電図(３時間３０分超４時間）
           03  FILLER          PIC X(12)  VALUE    "04 160129130".
      *    ホルター型心電図(４時間超４時間３０分）
           03  FILLER          PIC X(12)  VALUE    "04 160129230".
      *    ホルター型心電図(４時間３０分超５時間）
           03  FILLER          PIC X(12)  VALUE    "04 160129330".
      *    ホルター型心電図(５時間超５時間３０分）
           03  FILLER          PIC X(12)  VALUE    "04 160129430".
      *    ホルター型心電図(５時間３０分超６時間）
           03  FILLER          PIC X(12)  VALUE    "04 160129530".
      *    ホルター型心電図(６時間超６時間３０分）
           03  FILLER          PIC X(12)  VALUE    "04 160129630".
      *    ホルター型心電図(６時間３０分超７時間）
           03  FILLER          PIC X(12)  VALUE    "04 160129730".
      *    ホルター型心電図(７時間超７時間３０分）
           03  FILLER          PIC X(12)  VALUE    "04 160129830".
      *    ホルター型心電図(７時間３０分超８時間未満）
           03  FILLER          PIC X(12)  VALUE    "04 160129930".
      * 05
      *    ＰＧＣ
           03  FILLER          PIC X(12)  VALUE    "05 160071910".
      *    亜硝酸アミル吸入心音図
           03  FILLER          PIC X(12)  VALUE    "05 160072030".
      * 06
      *    気管支鏡
           03  FILLER          PIC X(12)  VALUE    "06 160092610".
      *    気管支カメラ
           03  FILLER          PIC X(12)  VALUE    "06 160092750".
      * 07
      *    食道鏡
           03  FILLER          PIC X(12)  VALUE    "07 160093210".
      *    食道カメラ
           03  FILLER          PIC X(12)  VALUE    "07 160093350".
      * 08
      *    胃鏡
           03  FILLER          PIC X(12)  VALUE    "08 160093510".
      *    ガストロカメラ
           03  FILLER          PIC X(12)  VALUE    "08 160093650".
      * 09
      *    直腸鏡
           03  FILLER          PIC X(12)  VALUE    "09 160093510".
      *    コロンブラッシュ法（沈)
           03  FILLER          PIC X(12)  VALUE    "09 160094330".
      *    コロンブラッシュ法（組織）
           03  FILLER          PIC X(12)  VALUE    "09 160094430".
      * 10
      *    ＥＦ−Ｓ状結腸
           03  FILLER          PIC X(12)  VALUE    "10 160094710".
      *    ＥＦ−下行結腸及び横行結腸
           03  FILLER          PIC X(12)  VALUE    "10 160094810".
      *    ＥＦ−下行結腸及び盲腸
           03  FILLER          PIC X(12)  VALUE    "10 160094910".
      * 11
      *    リアルタイム解析型心電図
           03  FILLER          PIC X(12)  VALUE    "11 160125550".
      *    携帯型発作時心電図記録計使用心電図
           03  FILLER          PIC X(12)  VALUE    "11 160125950".
      * 12
      *    超音波検査（断層撮影法）（腹部）
           03  FILLER          PIC X(12)  VALUE    "12 160072210".
      *    超音波検査（断層撮影法）（その他）
           03  FILLER          PIC X(12)  VALUE    "12 160165010".
      *    超音波（ＵＣＧ以外）（断層撮影法とＭモード法）
           03  FILLER          PIC X(12)  VALUE    "12 160072450".
      * 13
      *    超音波（ＵＣＧ以外）（断層撮影法とＭモード法）
      **** 03  FILLER          PIC X(12)  VALUE    "13 160072450".
      *    超音波（ＵＣＧ）（断層撮影法とＭモード法）
           03  FILLER          PIC X(12)  VALUE    "13 160072510".
      *    超音波（ＵＣＧ）（Ｍモード法）
           03  FILLER          PIC X(12)  VALUE    "13 160072610".
      *    超音波（ＵＣＧ以外）（Ｍモード法）
           03  FILLER          PIC X(12)  VALUE    "13 160072750".
      *    超音波（ＵＣＧ）（経食道的超音波法）
           03  FILLER          PIC X(12)  VALUE    "13 160160410".
      * 14
      *    超音波（ドップラー法）（胎児心音観察）
           03  FILLER          PIC X(12)  VALUE    "14 160072910".
      *    超音波（ドップラー法）（脳動脈血流速度連続測定）
           03  FILLER          PIC X(12)  VALUE    "14 160147110".
      *    超音波（ドップラー法）（脳動脈血流速度マッピング法）
           03  FILLER          PIC X(12)  VALUE    "14 160147210".
      *    超音波（ドップラー法）（末梢血管血行動態）
           03  FILLER          PIC X(12)  VALUE    "14 160150050".
      *
       01  TBL-KENSA-HOU-AREA-R    REDEFINES   TBL-KENSA-HOU-AREA.
      **** 03  TBL-KENSA-HOU-OCC       OCCURS  56  INDEXED  KEN-IDX.
           03  TBL-KENSA-HOU-OCC       OCCURS  65  INDEXED  KEN-IDX.
               05  TBL-KEN-HOU         PIC X(02).
               05  TBL-FILLER          PIC X(01).
               05  TBL-KEN-SRYCD       PIC X(09).
       01  KEN-IDX-MAX                 PIC 9(04)   VALUE   65.
      *
      *****************************************************************
      *    重複算定不可テーブル
      *****************************************************************
      *
       01  TBL-KENSA-JYU-AREA.
      *どれか２つ
      *    ＨＤＬコレステロール
           03  FILLER          PIC X(14)  VALUE    "01 2 160023410".
      *    総コレステロール
           03  FILLER          PIC X(14)  VALUE    "01 2 160022410".
      *    ＬＤＬコレステロール
           03  FILLER          PIC X(14)  VALUE    "01 2 160167250".
      *どれか２つ（Ｈ２０．４から ３つ算定可）
      **** ＴＩＢＣ
      *    03  FILLER          PIC X(14)  VALUE    "02 2 160023610".
      *    ＵＩＢＣ
      *    03  FILLER          PIC X(14)  VALUE    "02 2 160023710".
      *    鉄
      **** 03  FILLER          PIC X(14)  VALUE    "02 2 160022110".
      *どれか１つ
      *    Ｃａ
           03  FILLER          PIC X(14)  VALUE    "03 1 160021510".
      *    イオン化カルシウム
           03  FILLER          PIC X(14)  VALUE    "03 1 160021610".
      *どれか１つ
      *!!!!蛋白分画
      *    03  FILLER          PIC X(14)  VALUE    "04 1 160022810".
      *    Ａ／Ｇ
      *!!! 03  FILLER          PIC X(14)  VALUE    "04 11160022350".
      *
      *どれか１つ
      *    ＳＰ−Ａ
           03  FILLER          PIC X(14)  VALUE    "05 1 160169350".
      *    シリアル化糖鎖抗原ＫＬ−６
           03  FILLER          PIC X(14)  VALUE    "05 1 160168550".
      *    サーファクタントロプロテインＤ
           03  FILLER          PIC X(14)  VALUE    "05 1 160168450".
      *どれか２つ
      *    蛋白分画測定
           03  FILLER          PIC X(14)  VALUE    "06 2 160022810".
      *    総蛋白（ＴＰ）
           03  FILLER          PIC X(14)  VALUE    "06 2 160017410".
      *    アルブミン（Ａｌｂ）
           03  FILLER          PIC X(14)  VALUE    "06 2 160018910".
      *
       01  TBL-KENSA-JYU-AREA-R    REDEFINES   TBL-KENSA-JYU-AREA.
           03  TBL-KENSA-JYU-OCC       OCCURS  11  INDEXED  JYU-IDX.
               05  TBL-JYU-KBN         PIC X(02).
               05  TBL-FILLER1         PIC X(01).
               05  TBL-JYU-KEN         PIC X(01).
               05  TBL-FILLER2         PIC X(01).
               05  TBL-JYU-SRYCD       PIC X(09).
      *01  KEN-JYU-MAX                 PIC 9(04)   VALUE   11.
       01  KEN-JYU-MAX                 PIC 9(04)   VALUE   11.
      *
      *    重複算定不可テーブル（２）
       01  TBL-KENSA-JYU2-AREA.
      *どれか２つ（Ｈ１８．４から廃止）
      *    ＴＰ
           03  FILLER          PIC X(14)  VALUE    "01 2 160017410".
      *    Ａ／Ｇ
           03  FILLER          PIC X(14)  VALUE    "01 2 160022350".
      *    アルブミン
           03  FILLER          PIC X(14)  VALUE    "01 2 160018910".
      *
      *
       01  TBL-KENSA-JYU2-AREA-R    REDEFINES  TBL-KENSA-JYU2-AREA.
           03  TBL-KENSA-JYU2-OCC      OCCURS  3   INDEXED  JYU2-IDX.
               05  TBL2-JYU-KBN         PIC X(02).
               05  TBL2-FILLER1         PIC X(01).
               05  TBL2-JYU-KEN         PIC X(01).
               05  TBL2-FILLER2         PIC X(01).
               05  TBL2-JYU-SRYCD       PIC X(09).
       01  KEN-JYU2-MAX                 PIC 9(04)   VALUE   3.
      *
       01  WRK-JYUFUKU-CHK.
           03  WRK-JYU-CNT             PIC 9(02).
           03  WRK-JYU-IDX             PIC 9(04).
           03  WRK-JYU-KBN             PIC X(02).
           03  WRK-JYU-KEN             PIC 9(01).
           03  WRK-JYU-FLG             PIC X(01).
      *
      *    検体検査迅速実施加算対象
      *01  TBL-KENSA-TAI-AREA.
      *    03  FILLER          PIC X(09)  VALUE    "160000310".
      *    03  FILLER          PIC X(09)  VALUE    "160005010".
      *    03  FILLER          PIC X(09)  VALUE    "160006810".
      *    03  FILLER          PIC X(09)  VALUE    "160007610".
      *    03  FILLER          PIC X(09)  VALUE    "160008010".
      *    03  FILLER          PIC X(09)  VALUE    "160010010".
      *    03  FILLER          PIC X(09)  VALUE    "160012010".
      *    03  FILLER          PIC X(09)  VALUE    "160014510".
      *    03  FILLER          PIC X(09)  VALUE    "160114010".
      *    03  FILLER          PIC X(09)  VALUE    "160017010".
      *    03  FILLER          PIC X(09)  VALUE    "160017410".
      *    03  FILLER          PIC X(09)  VALUE    "160018910".
      *    03  FILLER          PIC X(09)  VALUE    "160019010".
      *    03  FILLER          PIC X(09)  VALUE    "160131950".
      *    03  FILLER          PIC X(09)  VALUE    "160019210".
      *    03  FILLER          PIC X(09)  VALUE    "160132150".
      *    03  FILLER          PIC X(09)  VALUE    "160019310".
      *    03  FILLER          PIC X(09)  VALUE    "160132250".
      *    03  FILLER          PIC X(09)  VALUE    "160020010".
      *    03  FILLER          PIC X(09)  VALUE    "160020210".
      *    03  FILLER          PIC X(09)  VALUE    "160020410".
      *    03  FILLER          PIC X(09)  VALUE    "160020910".
      *    03  FILLER          PIC X(09)  VALUE    "160021110".
      *    03  FILLER          PIC X(09)  VALUE    "160132450".
      *    03  FILLER          PIC X(09)  VALUE    "160021410".
      *    03  FILLER          PIC X(09)  VALUE    "160132650".
      *    03  FILLER          PIC X(09)  VALUE    "160021510".
      *    03  FILLER          PIC X(09)  VALUE    "160132750".
      *    03  FILLER          PIC X(09)  VALUE    "160019410".
      *    03  FILLER          PIC X(09)  VALUE    "160137950".
      *    03  FILLER          PIC X(09)  VALUE    "160138150".
      *    03  FILLER          PIC X(09)  VALUE    "160138750".
      *    03  FILLER          PIC X(09)  VALUE    "160019510".
      *    03  FILLER          PIC X(09)  VALUE    "160020610".
      *    03  FILLER          PIC X(09)  VALUE    "160023410".
      *    03  FILLER          PIC X(09)  VALUE    "160022410".
      *    03  FILLER          PIC X(09)  VALUE    "160022510".
      *    03  FILLER          PIC X(09)  VALUE    "160022610".
      *    03  FILLER          PIC X(09)  VALUE    "160167250".
      *    03  FILLER          PIC X(09)  VALUE    "160151050".
      *    03  FILLER          PIC X(09)  VALUE    "160031710".
      *    03  FILLER          PIC X(09)  VALUE    "160033210".
      *    03  FILLER          PIC X(09)  VALUE    "160033310".
      *    03  FILLER          PIC X(09)  VALUE    "160036510".
      *    03  FILLER          PIC X(09)  VALUE    "160036710".
      *    03  FILLER          PIC X(09)  VALUE    "160037510".
      *    03  FILLER          PIC X(09)  VALUE    "160037210".
      *    03  FILLER          PIC X(09)  VALUE    "160054710".
      *    03  FILLER          PIC X(09)  VALUE    "160057710".
      *
      *01  TBL-KENSA-TAI-AREA-R    REDEFINES  TBL-KENSA-TAI-AREA.
      *    03  TBL-KENSA-TAI-OCC        OCCURS  49  INDEXED  TAI-IDX.
      *        05  TBL-TAI-SRYCD        PIC X(09).
      *01  KEN-TAI-MAX                  PIC 9(04)   VALUE   49.
      *
      *    検体検査迅速実施加算対象
           COPY    "CMGAIKENSATBL.INC".
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    酸素コードテーブル
           COPY     "CMSANSOTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ（施設基準）
           COPY    "CPSK1006.INC".
      *    システム管理マスタ（外来迅速検体検査情報）
           COPY    "CPSK1016.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
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
           COPY    "CPORCSC94.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    表示名称編集パラメタ領域
           COPY    "CPORCSMEIHEN.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
      *    診療行為時、ＳＰＡを編集
           IF      LNK-ORCSC-PG        =   "K02"
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           ELSE
               INITIALIZE                  SPA-K01KYOUTU
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
               GO      TO      100-SEL-SYORI-EXT
           END-IF
      *
      *    点数計算
           PERFORM 1002-TENSU-KEI-SEC
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
           MOVE    ZERO                TO  IDX-SANSO
           MOVE    ZERO                TO  IDX-SYU
           MOVE    ZERO                TO  IDX-TEIGEN
           MOVE    ZERO                TO  IDX-TEIGEN2
           INITIALIZE                  WRK-HOUKNS-AREA
           INITIALIZE                  WRK-AREA
           MOVE    ZERO                TO  FLG-KIJUNN
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE AND
                               WRK-COM-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."
                                                       OR  "*"
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
      *************WHEN    "."
                   WHEN    "*"
                       CONTINUE
                   WHEN    "."
                       PERFORM 10010-SRYKBN-CHK-SEC
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10011-CHK-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    酸素の補正率を自動発生する
           IF     (WRK-ERRCD           =   SPACE)   AND
                  (IDX-SANSO           >   ZERO )
               PERFORM 10011-SANSO-ADD-SEC
           END-IF
      *
      *    逓減加算算定チェック（診療行為入力時のみ）
           IF     (IDX-TEIGEN          >   ZERO )  AND
                  (LNK-ORCSC-PG        =   "K02"
                                       OR  "KT01" )
               PERFORM 1008-TEIGEN-SYORI-SEC
           END-IF
      *
           MOVE    WRK-ERRCD           TO  SPA-ERRCD
      *
           .
       1001-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    投薬の処方のみチェック処理
      *****************************************************************
       10010-SRYKBN-CHK-SEC             SECTION.
      *
      *?   EVALUATE    WRK-GMN-INPUTCD(IDX-Z)(2:3)
      *        WHEN    "610"
      *            IF      SPA-NYUGAIKBN   =   "1"
      *                MOVE    "9028"          TO  WRK-ERRCD
      *                MOVE    "1"             TO  WRK-COM-CUR
      *                                                      (IDX-Z)
      *            END-IF
      *?   END-EVALUATE
           .
       10010-SRYKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細別内容チェック、自動算定編集処理
      *****************************************************************
       10011-CHK-SEC                  SECTION.
      *
      *    各チェック処理
           EVALUATE    WRK-MOJI
               WHEN    "1"
      *            手技料 加算チェック
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "2"
                       PERFORM 1005-KASAN-CHK-SEC
                   END-IF
      *            同一剤内に同じ検査がないこと
                   IF     (SPA-ERRCD           =   SPACE ) AND
                          (WRK-COM-DATAKBN(IDX-Z)  =   "1" )
                       PERFORM 100114-JYUFUKUCHEK-SEC
                   END-IF
      *            逓減加算算定チェック
                   IF     (WRK-COM-DATAKBN (IDX-Z)     =   "1" )  AND
                          (WRK-COM-TEIGENKBN (IDX-Z)   =   1   )
                       MOVE    IDX-Z             TO  IDX-TEIGEN
                   END-IF
                   IF     (WRK-COM-DATAKBN (IDX-Z)     =   "1" )  AND
                          (WRK-COM-TEIGENKBN (IDX-Z)   =   1   )
                       MOVE    IDX-Z             TO  IDX-TEIGEN2
                   END-IF
      *
      *----(01.00.03) LINE ADD START ----------------------------------
      *            年齢加算算定チェック
                   MOVE    SPACE               TO  WRK-NYU-SRYCD
                   PERFORM 1007-NENREI-SYORI-SEC
      *----(01.00.03) LINE ADD END   ----------------------------------
      *            施設基準不適合チェック
                   IF      WRK-COM-KIJUNTEIGENKBN(IDX-Z)
                                               NOT =   ZERO
                       PERFORM 100111-KIJUNN-CHK-SEC
                   END-IF
      *            神経磁気診断のコメント自動発生
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "160175810"
                       PERFORM 100112-COMMNT-SET-SEC
                   END-IF
      *
      *            入院患者初回加算のチェック
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "160161510"
                       PERFORM 100113-NYUINSYOKAI-SEC
                   END-IF
      *            前回検査日自動発生
                   IF      WRK-COM-ZENKNSKBN (IDX-Z)   =   "1"
                                                       OR  "2"
                       IF      LNK-ORCSC-PG        =   "K02"
                                                   OR  "KT01"
                           PERFORM 100115-ZENKNSCOMMNT-SET-SEC
                       END-IF
                   END-IF
      *            初回コンタクトレンズ検査チェック
      *Ｈ２０．４から削除
      *********    IF     (SPA-ERRCD           =   SPACE ) AND
      *                   (LNK-ORCSC-PG        =   "K02" ) AND
      *                   (WRK-COM-INPUTCD (IDX-Z) =   "160179210"
      *                                            OR  "160179410") AND
      *                   (WRK-COM-KZMFLG (IDX-Z)  =   SPACE    ) 
      *                PERFORM 100115-CONTAKUCHEK-SEC
      ********     END-IF
      *        器材
               WHEN    "7"
      *            酸素区分判定
                   IF      WRK-COM-SANSOKBN (IDX-Z)    =   1
      *                酸素の時、酸素補正率を自動加算
                       MOVE    WRK-COM-INPUTCD (IDX-Z) TO  WRK-SRYCD
                       PERFORM 100115-SANSO-CHK-SEC
                       IF      FLG-SANSO-OK        =   1
                           MOVE    IDX-Z           TO  IDX-SANSO
                       ELSE
                           IF      IDX-SANSO        =   ZERO
                               MOVE    "0038"       TO  WRK-ERRCD
                               MOVE    "1"          TO  WRK-COM-CUR
                                                              (IDX-Z)
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       1001-SYORI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素判定処理
      *****************************************************************
       100115-SANSO-CHK-SEC           SECTION.
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
      *    酸素補正率自動加算
           MOVE    "770020070"         TO  WRK-SRYCD
           MOVE    IDX-SANSO           TO  IDX-S
           MOVE    ZERO                TO  FLG-KASAN
           PERFORM 2001-JIDOTUIKA-SEC
           IF      FLG-ARI             =   1
               MOVE    "2"             TO  WRK-COM-AUTOKBN (IDX-T)
           END-IF
      *
           .
       10011-SANSO-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準不適合チェック加算処理
      *****************************************************************
       100111-KIJUNN-CHK-SEC           SECTION.
      *
      *    手技の時、適合チェック
           IF      WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   2  OR  3
               PERFORM 1001111-SSTKIJUN-CHK-SEC
      *        施設基準適応
               IF      FLG-ERR             =   ZERO
                   MOVE    1               TO  FLG-KIJUNN
                   GO      TO      100111-KIJUNN-CHK-EXT
               END-IF
      *        施設基準不適応
               IF     (WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   2 )  OR
                     ((WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   3   ) AND
                      (LNK-ORCSC-NENREI-YY             =   ZERO))
                   MOVE    2                   TO  FLG-KIJUNN
      *            施設基準不適合（検査）
                   MOVE    IDX-Z               TO  IDX-S
                   MOVE    "160174470"         TO  WRK-SRYCD
                   IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
                       CONTINUE
                   ELSE
                       MOVE    ZERO            TO  FLG-KASAN
                       PERFORM 2001-JIDOTUIKA-SEC
                  END-IF
              END-IF
           END-IF
      *
      *    加算の時、対象の手技が有ること
           IF      WRK-COM-KIJUNTEIGENKBN (IDX-Z)  =   1
               IF      FLG-KIJUNN          =   ZERO
                   MOVE    "0073"          TO  WRK-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
           END-IF
           .
       100111-KIJUNN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設基準不適応チェック処理
      *****************************************************************
       1001111-SSTKIJUN-CHK-SEC        SECTION.
      *
      *    基準不適合逓減対象施設基準
           MOVE    WRK-COM-KIJUNTEIGENCD (IDX-Z)
                                       TO  IDY
      *
           MOVE    ZERO                TO  FLG-ERR
      *    施設基準情報検索
           MOVE    SPACE               TO  SYS-1006-REC
           MOVE    "1006"              TO  SYS-1006-KANRICD
           IF      IDY                 <=  500
               MOVE    "1"                 TO  SYS-1006-KBNCD
           ELSE
               MOVE    "2"                 TO  SYS-1006-KBNCD
           END-IF
           MOVE    SPA-SRYYMD          TO  SYS-1006-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1006-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1006-HOSPNUM
           MOVE    SYS-1006-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1006-REC
           ELSE
               INITIALIZE                  SYS-1006-REC
           END-IF
      *
      *    基準不適合逓減対象施設基準
           MOVE    WRK-COM-KIJUNTEIGENCD (IDX-Z)
                                       TO  IDY
           IF      IDY                 <=  500
               IF      SYS-1006-SSTKIJUN (IDY) =   1
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-ERR
               END-IF
           ELSE
               IF      SYS-1006-SSTKIJUN (IDY - 500) =   1
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-ERR
               END-IF
           END-IF
      *
           .
       1001111-SSTKIJUN-CHK-EXT.
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
           END-PERFORM
      *    検査加算料
           IF     (WRK-COM-SRYSYUKBN(IDX-Z)    =   "603" )  AND
                  (WRK-COM-SRYKBN (IDX-Z)      =   "60"  )
               MOVE    1                   TO  FLG-ARI
           END-IF
      *    検査加算料
           IF     (WRK-COM-SRYSYUKBN(IDX-Z)    =   "643" )  AND
                  (WRK-COM-SRYKBN (IDX-Z)      =   "64"  )
               MOVE    1                   TO  FLG-ARI
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
      *    年齢加算追加処理
      *****************************************************************
       1007-NENREI-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                 TO  IDX-S
      *    通則年齢区分があり、３歳未満の時
           IF     (WRK-COM-TSUSOKUAGE (IDX-Z)  =   "1")  AND
                  (WRK-COM-DATAKBN (IDX-Z)     =   "1")  AND
                  (LNK-ORCSC-NENREI-YY         <=  3   )
      *        新生児加算（生体検査）
               MOVE    "160155290"         TO  WRK-SRYCD
               MOVE    IDX-Z               TO  IDX-S
               MOVE    1                   TO  FLG-KASAN
               PERFORM 2001-JIDOTUIKA-SEC
               IF      FLG-ARI             =   1
                   MOVE    "2"                 TO  WRK-COM-AUTOKBN
                                                               (IDX-T)
                   MOVE    IDX-T               TO  IDX-S
               ELSE
      *            乳幼児加算（生体検査）
                   MOVE    "160155390"         TO  WRK-SRYCD
                   MOVE    IDX-Z               TO  IDX-S
                   MOVE    1                   TO  FLG-KASAN
                   PERFORM 2001-JIDOTUIKA-SEC
                   IF      FLG-ARI             =   1
                       MOVE    "2"                 TO  WRK-COM-AUTOKBN
                                                               (IDX-T)
                       MOVE    IDX-T               TO  IDX-S
                   END-IF
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  IDX-NYU
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-Z IDX2)
                                          =   SPACE  OR  ZERO )
      *        加算追加
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-Z IDX2)  TO  WRK-SRYCD
               IF      IDX-S               =   ZERO
                   MOVE    IDX-Z               TO  IDX-S
               END-IF
               MOVE    ZERO                TO  FLG-KASAN
               PERFORM 2001-JIDOTUIKA-SEC
               IF      FLG-ARI             =   1
                   MOVE    "2"                 TO  WRK-COM-AUTOKBN
                                                               (IDX-T)
                   MOVE    4                   TO  IDX2
                   MOVE    WRK-SRYCD           TO  WRK-NYU-SRYCD
               END-IF
           END-PERFORM
      *
      *
           .
       1007-NYUYOJI-SET-EXT.
           EXIT.
      *****************************************************************
      *    逓減加算算定チェック加算処理
      *****************************************************************
       1008-TEIGEN-SYORI-SEC           SECTION.
      *
      *    逓減加算算定チェック
           PERFORM 100812-TEIGEN-CHK-SEC
      *
      *    逓減加算自動追加
           IF      WRK-COM-TEIGEN-FLG(IDX-TEIGEN)  =   1 OR 2
               MOVE    "160000190"         TO  WRK-SRYCD
               MOVE    IDX-TEIGEN2         TO  IDX-S
               MOVE    2                   TO  FLG-KASAN
               PERFORM 2001-JIDOTUIKA-SEC
               MOVE    "2"                 TO  WRK-COM-AUTOKBN
                                                           (IDX-T)
               MOVE    IDX-T               TO  IDX-Z
      *
           END-IF
      *
           .
       1008-TEIGEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     逓減加算算定チェック処理
      *****************************************************************
       100812-TEIGEN-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-TODAY-CNT
           MOVE    ZERO                TO  WRK-TODAY-CNTG
           MOVE    ZERO                TO  FLG-CHK
      *
           IF      WRK-COM-TEIGEN-CHK(IDX-TEIGEN)   =   ZERO
      *        算定歴チェック
               MOVE    WRK-COM-INPUTCD (IDX-TEIGEN) TO  WRK-SRYCD
               PERFORM 108121-SANTEI-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      100812-TEIGEN-CHK-EXT
               END-IF
      *        逓減加算自動追加あり
               MOVE    1                   TO  WRK-COM-TEIGEN-CHK
                                                       (IDX-TEIGEN)
               IF      WRK-SANTEI-CNT      >   ZERO
                   MOVE    1           TO  WRK-COM-TEIGEN-FLG
                                                    (IDX-TEIGEN)
               END-IF
               ADD     WRK-TODAY-CNTG      TO  WRK-TODAY-CNT
               MOVE    1                   TO  FLG-CHK
           END-IF
      *
      *    同一検査算定有無の判定
           SET     KEN-IDX             TO  1
           SEARCH      TBL-KENSA-HOU-OCC   VARYING   KEN-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-KEN-HOU
                   WHEN    WRK-COM-INPUTCD (IDX-TEIGEN)
                                               =   TBL-KEN-SRYCD
                                                           (KEN-IDX)
                       MOVE    TBL-KEN-HOU  (KEN-IDX)
                                                TO  WRK-KEN-HOU
           END-SEARCH
      *    検査毎にテーブル編集をする
           MOVE    ZERO            TO  IDX-TEI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   20  )  OR
                              (IDX-TEI     >   ZERO)
               IF      WRK-KEN-HOU         =   SPACE
                   IF     (LNK-ORCSC-TEIGEN-SRYCD(IDX)  =   SPACE )  AND
                          (LNK-ORCSC-TEIGEN-HOU  (IDX)  =   SPACE )
                       MOVE    WRK-COM-INPUTCD (IDX-TEIGEN)
                                           TO  LNK-ORCSC-TEIGEN-SRYCD
                                                                (IDX)
                   END-IF
                   IF      LNK-ORCSC-TEIGEN-SRYCD(IDX)  =
                                               WRK-COM-INPUTCD
                                                      (IDX-TEIGEN)
                       MOVE    IDX             TO  IDX-TEI
                   END-IF
               ELSE
                   IF     (LNK-ORCSC-TEIGEN-SRYCD(IDX)  =   SPACE )  AND
                          (LNK-ORCSC-TEIGEN-HOU  (IDX)  =   SPACE )
                       MOVE    WRK-KEN-HOU     TO  LNK-ORCSC-TEIGEN-HOU
                                                          (IDX)
                   END-IF
                   IF      LNK-ORCSC-TEIGEN-HOU(IDX)  =   WRK-KEN-HOU
                       MOVE    IDX             TO  IDX-TEI
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-KEN-HOU         =   SPACE
               ADD     WRK-SANTEI-CNT      TO  LNK-ORCSC-TEIGEN-SANTEI
                                                         (IDX-TEI)
           ELSE
               IF      LNK-ORCSC-TEIGEN-CNT (IDX-TEI) =   ZERO
                   ADD     WRK-SANTEI-CNT  TO  LNK-ORCSC-TEIGEN-SANTEI
                                                           (IDX-TEI)
               END-IF
           END-IF
      *
      *    同一算定検査がある時、すべての検査回数を算定する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   KEN-IDX-MAX)  OR
                              (WRK-KEN-HOU     =   SPACE  )  OR
                              (FLG-CHK         =   ZERO   )  OR
                              (SPA-ERRCD   NOT =   SPACE  )  OR
                              (WRK-SANTEI-CNT  >   ZERO   )
               SET     KEN-IDX             TO  IDX
               IF      TBL-KEN-HOU (KEN-IDX)   =   WRK-KEN-HOU
                   MOVE    TBL-KEN-SRYCD (KEN-IDX) TO  WRK-SRYCD
                   PERFORM 108121-SANTEI-CHK-SEC
                   ADD     WRK-SANTEI-CNT      TO
                                               LNK-ORCSC-TEIGEN-SANTEI
                                                       (IDX-TEI)
                   ADD     WRK-TODAY-CNTG      TO  WRK-TODAY-CNT
               END-IF
           END-PERFORM
      *    当月最初で、２回以上はエラーとする
           IF     (LNK-ORCSC-TEIGEN-SANTEI(IDX-TEI)    =   ZERO )  AND
                  (LNK-ORCSC-TEIGEN-CNT   (IDX-TEI)    =   ZERO )  AND
                  (WRK-COM-TEIGEN-FLG (IDX-TEIGEN)     =   ZERO )  AND
                  (LNK-ORCSC-KAISU                 >   1     OR
                   WRK-COM-NYU-KAI(IDX-TEIGEN)     >   1    )
               MOVE    "0096"              TO  WRK-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-TEIGEN)
               GO      TO      100812-TEIGEN-CHK-EXT
           END-IF
      *    今回入力分
           IF     (SPA-NYUGAIKBN       =   "1" )
              OR  (LNK-ORCSC-PG        =   "KT01" )
              ADD      WRK-COM-NYU-KAI(IDX-TEIGEN)
                                           TO  LNK-ORCSC-TEIGEN-CNT
                                                              (IDX-TEI)
           ELSE
               ADD     LNK-ORCSC-KAISU     TO  LNK-ORCSC-TEIGEN-CNT
                                                             (IDX-TEI)
           END-IF
      *
           IF      LNK-ORCSC-TEIGEN-SANTEI(IDX-TEI) >   ZERO
               MOVE    1           TO  WRK-COM-TEIGEN-FLG(IDX-TEIGEN)
               GO      TO      100812-TEIGEN-CHK-EXT
           END-IF
           IF      WRK-COM-TEIGEN-FLG(IDX-TEIGEN)  =   1
               IF     (WRK-TODAY-CNT               =   ZERO )  AND
                      (FLG-CHK                     =   1    )
                   MOVE    ZERO    TO  WRK-COM-TEIGEN-FLG(IDX-TEIGEN)
               ELSE
                   GO      TO      100812-TEIGEN-CHK-EXT
               END-IF
           END-IF
      *
           IF      LNK-ORCSC-TEIGEN-CNT(IDX-TEI)    >   1
               MOVE    2           TO  WRK-COM-TEIGEN-FLG(IDX-TEIGEN)
           ELSE
               MOVE    ZERO        TO  WRK-COM-TEIGEN-FLG(IDX-TEIGEN)
           END-IF
           .
      *
       100812-TEIGEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックコードの算定チェック
      *****************************************************************
       108121-SANTEI-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
      *    点数マスタ読込
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               GO      TO      108121-SANTEI-CHK-EXT
           END-IF
      *    外来まとめはＳＰＡに編集
           IF      LNK-ORCSC-KT01FLG    =   "1"
               PERFORM 2005121-KT01-SANTEI-SEC
               GO      TO      108121-SANTEI-CHK-EXT
           END-IF
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
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
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SANTEI          =   1
               INITIALIZE              SANTEI-REC
           END-IF
      *
      *    算定歴の回数をチェックする
           MOVE    SANTEI-MSANTEICNT   TO  WRK-SANTEI-CNT
           IF      SANTEI-FSANTEIYMD   >   SPA-SRYYMD
               MOVE    "0056"          TO  SPA-ERRCD
           END-IF
      *
      *    訂正の時、訂正前までカウントする
           IF      LNK-ORCSC-SYORIFLG  =   1
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
               MOVE    ZERO                TO  WRK-SANTEI-CNT
               PERFORM VARYING     IDX-Y3      FROM    1   BY  1
                       UNTIL       IDX-Y3      >=  IDX-Y2
                   ADD     SANTEI-DAY (IDX-Y3) TO  WRK-SANTEI-CNT
               END-PERFORM
      *        当日加算分
               MOVE    SANTEI-DAY (IDX-Y2) TO  WRK-TODAY-CNTG
      *        前回分集計
               MOVE    ZERO                TO  WRK-TEI-CNT
               PERFORM VARYING     IDT     FROM    1   BY  1
                       UNTIL      (IDT     >   SPA-MAX-LINE )  OR
                                  (LNK-ORCSC-TEI-SRYCD(IDT)
                                           =   SPACE )
                   IF      WRK-SRYCD       =   LNK-ORCSC-TEI-SRYCD(IDT)
                       ADD     LNK-ORCSC-TEI-KAISU(IDT)
                                           TO  WRK-TEI-CNT
                   END-IF
               END-PERFORM
               IF      WRK-TODAY-CNTG      >   WRK-TEI-CNT
                   COMPUTE WRK-TODAY-CNTG      =   WRK-TODAY-CNTG
                                               -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO                TO  WRK-TODAY-CNTG
               END-IF
           END-IF
           .
       108121-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来まとめ処理
      *****************************************************************
       2005121-KT01-SANTEI-SEC      SECTION.
      *
           PERFORM VARYING     IDX-SN      FROM    1   BY  1
                   UNTIL      (IDX-SN      >   100 )   OR
                              (LNK-ORCSC-KT01-SRYCD  (IDX-SN) =   SPACE)
               IF      WRK-SRYCD       =   LNK-ORCSC-KT01-SRYCD(IDX-SN)
                   INITIALIZE                  SANTEI-REC
                   MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
      *            当日まで対象とする
                   PERFORM VARYING     IDD         FROM    1   BY  1
                           UNTIL      (IDD         >   IDX-Y2 )
                       MOVE    LNK-ORCSC-KT01-DAY (IDX-SN IDD)
                                                   TO  SANTEI-DAY (IDD)
                       ADD     SANTEI-DAY (IDD)    TO  SANTEI-MSANTEICNT
                   END-PERFORM
                   MOVE    SANTEI-MSANTEICNT   TO  WRK-SANTEI-CNT
               END-IF
           END-PERFORM
           .
       2005121-KT01-SANTEI-EXT.
           EXIT.
      *****************************************************************
      *    神経磁気診断のコメント自動発生処理
      *****************************************************************
       100112-COMMNT-SET-SEC                  SECTION.
      *
           MOVE    "840000090"         TO  WRK-SRYCD
           MOVE    IDX-Z               TO  IDX-S
           MOVE    4                   TO  FLG-KASAN
           PERFORM 2001-JIDOTUIKA-SEC
           IF      FLG-ARI             =   1
               MOVE    "2"                 TO  WRK-COM-AUTOKBN(IDX-T)
               MOVE    "1"                 TO  WRK-COM-IN  (IDX-T)
               MOVE    "2"                 TO  WRK-COM-IN2 (IDX-T)
           END-IF
      *
           .
       100112-COMMNT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院患者初回加算のチェック処理
      *****************************************************************
       100113-NYUINSYOKAI-SEC          SECTION.
      *
           IF      SPA-NYUGAIKBN       =   "2"
               GO      TO      100113-NYUINSYOKAI-EXT
           END-IF
      *
           INITIALIZE                      ORCSC91AREA
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
           MOVE    "9"                 TO  LNK-SC91-KBN
           MOVE    1                   TO  LNK-SC91-NYUCNT
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
               MOVE    "9031"              TO  WRK-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *   １回のみ
           IF   (WRK-COM-NYU-KAI (IDX-Z)   >   1 )  OR
                (LNK-ORCSC-KAISU           >   1 )
               MOVE    "9035"              TO  WRK-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
           .
       100113-NYUINSYOKAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回検査日自動発生処理
      *****************************************************************
       100115-ZENKNSCOMMNT-SET-SEC                  SECTION.
      *
      *    剤内のコメント判定
           MOVE    ZERO                TO  FLG-ARI
           COMPUTE IDX4                =   IDX-Z       +   1
           PERFORM VARYING     IDX1    FROM    IDX4    BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     ) OR
                              (FLG-ARI                 =   1    ) OR
                             ((WRK-COM-INPUTCD (IDX1)  =   SPACE ) AND
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE ))
               IF      (WRK-COM-INPUTCD (IDX1)(1:1)    =   "8"  )
                    OR (WRK-COM-INPUTCD (IDX1)(1:3)    =   "008" )
                    MOVE    1              TO  FLG-ARI
               END-IF
               IF      (WRK-COM-INPUTCD (IDX1)(1:1)    =   "1" )
                    MOVE   SPA-MAX-LINE        TO  IDX1
               END-IF
           END-PERFORM
           IF      FLG-ARI             =   ZERO
               PERFORM 1001151-ZENSANTEICHK-SEC
           END-IF
           .
       100115-ZENKNSCOMMNT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査前回算定チェック処理
      *****************************************************************
       1001151-ZENSANTEICHK-SEC                  SECTION.
      *
      *    前回算定日判定
           MOVE    WRK-COM-INPUTCD (IDX-Z)
                                           TO  WRK-SRYCD
           PERFORM 1001151-ZENSANTEI-SET-SEC
      *
           IF      FLG-SYOKAI-OK       =   1
      *        前回あり
               MOVE    "840000087"         TO  WRK-SRYCD
           ELSE
      *        初回
               IF     WRK-COM-ZENKNSKBN (IDX-Z)    =   "1"
                   MOVE    "810000001"         TO  WRK-SRYCD
               ELSE
                   MOVE    "840000085"         TO  WRK-SRYCD
                   MOVE    SPA-SRYYMD          TO  WRK-YMD
               END-IF
           END-IF
           MOVE    IDX-Z               TO  IDX-S
           MOVE    5                   TO  FLG-KASAN
           PERFORM 2001-JIDOTUIKA-SEC
           IF      FLG-ARI             =   1
               MOVE    "1"             TO  WRK-COM-IN  (IDX-T)
               MOVE    "2"             TO  WRK-COM-AUTOKBN (IDX-T)
      *        診療内容編集
               IF      WRK-SRYCD           =   "810000001"
                   MOVE    "（初回実施）"      TO  WRK-GMN-MEISYO-G
                                                          (IDX-T)
                   MOVE    "1"                 TO  WRK-COM-MEIFLG
                                                          (IDX-T)
                   MOVE    SPACE           TO  WRK-COM-IN  (IDX-T)
               ELSE
                   MOVE    WRK-MM          TO  WRK-COM-ATAI1 (IDX-T)
                   MOVE    WRK-DD          TO  WRK-COM-ATAI2 (IDX-T)
               END-IF
      *        画面再編集
               INITIALIZE                  ORCSC94AREA
               MOVE    "1"                 TO  LNK-SC94-KBN
               MOVE    LNK-ORCSC-PG        TO  LNK-SC94-PG
               MOVE    IDX-T               TO  LNK-SC94-IDX
               CALL    "ORCSC94"           USING
                                           ORCSC94AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *
      *        診療日を編集
               INITIALIZE                  ORCSC92AREA
               MOVE    SPACE               TO  LNK-SC92-KBN
               MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
               MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
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
           END-IF
           .
       1001151-ZENSANTEICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    初回コンタクトレンズ検査チェック処理
      *****************************************************************
      *100115-CONTAKUCHEK-SEC                  SECTION.
      *
      *    MOVE    WRK-COM-INPUTCD (IDX-Z)
      *                                TO  WRK-SRYCD
      *    PERFORM 1001151-CONT-SANTEICHK-SEC
      *    IF      SPA-ERRCD           =   SPACE
      *        IF      WRK-COM-INPUTCD (IDX-Z) =   "160179210"
      *            コンタクトレンズ検査１（既装用者）
      *            MOVE    "160179310"         TO  WRK-SRYCD
      *        ELSE
      *            コンタクトレンズ検査２（既装用者）
      *            MOVE    "160179510"         TO  WRK-SRYCD
      *        END-IF
      *        PERFORM 1001151-CONT-SANTEICHK-SEC
      *    END-IF
      *    .
      *100115-CONTAKUCHEK-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    前回算定日判定処理
      *****************************************************************
       1001151-ZENSANTEI-SET-SEC                  SECTION.
      *
           MOVE    ZERO               TO  WRK-TEI-CNT
           IF      LNK-ORCSC-SYORIFLG  =   1
               PERFORM VARYING     IDT     FROM    1   BY  1
                       UNTIL      (IDT     >   SPA-MAX-LINE )  OR
                                  (LNK-ORCSC-TEI-SRYCD(IDT)
                                           =   SPACE )
                   IF      WRK-SRYCD       =   LNK-ORCSC-TEI-SRYCD(IDT)
                       ADD     LNK-ORCSC-TEI-KAISU(IDT)
                                           TO  WRK-TEI-CNT
                   END-IF
               END-PERFORM
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
           PERFORM UNTIL   (FLG-SANTEI     =   1   )  OR
                           (FLG-SYOKAI-OK  =   1   )
               MOVE    SANTEI-SRYYM        TO  WRK-YM
               MOVE    ZERO                TO  WRK-DD
               PERFORM VARYING    IDX2     FROM    31  BY  -1
                       UNTIL     (IDX2     <   1   )  OR
                                 (FLG-SYOKAI-OK    =   1   )
                   IF      SANTEI-DAY (IDX2)   NOT =   ZERO
                       MOVE    IDX2            TO  WRK-DD
      *                診療日以前の算定を有効とする
                       IF     (WRK-YMD             <=  SPA-SRYYMD)
                           MOVE    1               TO  FLG-SYOKAI-OK
                       END-IF
      *                訂正の時当日分は算定なしとする
                       IF     (LNK-ORCSC-SYORIFLG  =   1  )  AND
                              (WRK-YMD             =   SPA-SRYYMD) AND
                              (WRK-TEI-CNT         >   ZERO )
                           MOVE    ZERO        TO  FLG-SYOKAI-OK
                       END-IF
                   END-IF
               END-PERFORM
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1001151-ZENSANTEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初回コンタクトレンズ検査チェック処理
      *****************************************************************
      *1001151-CONT-SANTEICHK-SEC                  SECTION.
      *
      *    前回算定日判定
      *    PERFORM 1001151-ZENSANTEI-SET-SEC
      *
      *    IF      FLG-SYOKAI-OK       =   1
      *        IF      LNK-ORCSC-KEI-IDX   =   ZERO
      *            MOVE    IDX-Z           TO  LNK-ORCSC-KEI-IDX
      *            MOVE    "1"             TO  WRK-COM-KZMFLG (IDX-Z)
      *            MOVE    "K600"          TO  WRK-ERRCD
      *            MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
      *            発生日
      *            INITIALIZE                      STS-AREA-DAY
      *            INITIALIZE                      LNK-DAY2-AREA
      *            MOVE    "21"            TO  LNK-DAY2-IRAI
      *            MOVE    WRK-YMD         TO  LNK-DAY2-YMD
      *            CALL    "ORCSDAY"       USING   STS-AREA-DAY
      *                                            LNK-DAY2-AREA
      *            STRING  "算定日："          DELIMITED  BY  SIZE
      *                    LNK-DAY2-EDTYMD3    DELIMITED  BY  SIZE
      *                   INTO                 LNK-ORCSC-ERRMSG
      *            END-STRING
      *        END-IF
      *    END-IF
      *    .
      *1001151-CONT-SANTEICHK-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    同一検査チェック処理
      *****************************************************************
       100114-JYUFUKUCHEK-SEC                  SECTION.
      *
      *    入院か包括外の時
      *!!  IF     (SPA-NYUGAIKBN       =   "1"   )  OR
      *           (WRK-COM-SRYSYUKBN (IDX-Z)
      *!!                              =   "610" )
           IF     (SPA-NYUGAIKBN       =   "1"   )  AND
                  (WRK-COM-SRYSYUKBN (IDX-Z)
                                   NOT =   "610" )
             AND  (LNK-ORCSC-KENSAFLG  =   ZERO   )
               PERFORM VARYING     IDX1    FROM    1   BY  1
                       UNTIL       IDX1    >=  IDX-Z
                   IF      WRK-COM-INPUTCD(IDX1)
                                           =   WRK-COM-INPUTCD(IDX-Z)
      *             検体検査コメントの判定
                     AND  (WRK-COM-KENTAICOMMENT(IDX-Z)    =   ZERO  )
                     AND  (WRK-COM-KENGRP(IDX1)
                                           =   WRK-COM-KENGRP(IDX-Z))
                       MOVE    "0064"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
                   END-IF
               END-PERFORM
           END-IF
           .
       100114-JYUFUKUCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算編集処理
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-SYU
      *
           INITIALIZE                  WRK-HOUKNS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ORCSC90AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-GMN-KEI   (IDX-Z)
      *
      *        手技毎
               INITIALIZE                   WRK-COM-PLUS-AREA(IDX-Z)
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."  OR  "S"
                                                       OR  "*"
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
                   WHEN    "S"
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    OTHER
      *                編集チェックへ
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
      *        包括検査名称を元へ
               IF      WRK-MOJI            =   "1"
                   MOVE    WRK-COM-NAME(IDX-Z)  TO  WRK-GMN-MEISYO
                                                            (IDX-Z)
               END-IF
           END-PERFORM
      *
      *    最終行
      *    手技料点数合計計算（包括）
           PERFORM 1004-HOUKNS-SET-SEC
      *    割合加算・減算
           PERFORM 1006-KASAN-SET-SEC
      *
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    ZERO                TO  LNK-SC90-IDX
           MOVE    1                   TO  LNK-SC90-IDX-STR
           MOVE    IDX-MAX             TO  LNK-SC90-IDX-END
           CALL    "ORCSC90TEN"        USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *    計
           COMPUTE WRK-COM-KEI (IDX-MAX)   =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
      *
           MOVE    WRK-COM-KEI  (IDX-MAX)  TO  LNK-ORCSC-ZAIKEI
           MOVE    WRK-COM-TENSU(IDX-MAX)  TO  LNK-ORCSC-ZAITEN
           .
       1002-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数計算処理
      *****************************************************************
       10021-KEISAN-SEC                  SECTION.
      *
      *    各チェック処理
           EVALUATE    WRK-MOJI
               WHEN    "1"
      *            手技料点数加算
                   PERFORM 1003-HOUKNS-HEN-SEC
           END-EVALUATE
      *    回数
           MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU (IDX-Z)
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括チェック・点数集計処理
      *****************************************************************
       1003-HOUKNS-HEN-SEC                  SECTION.
      *
      *    ％加算・減算は集計しない
           IF     (WRK-COM-TENSIKIBETU (IDX-Z) =   5
                                               OR  6 )  OR
      *        検体検査コメントの判定
                  (WRK-COM-KENTAICOMMENT(IDX-Z)    =   1)
               CONTINUE
           ELSE
               EVALUATE    WRK-COM-HOUKNSKBN(IDX-Z)
      *DD          WHEN    01   THRU   08
      *内分泌負荷試験は包括計算外
                   WHEN    01   THRU   07
                   WHEN    09   THRU   10
                       MOVE    WRK-COM-HOUKNSKBN(IDX-Z)    TO  IDX
                       ADD     1               TO  WRK-HOUKNS-CNT (IDX)
                       ADD     WRK-COM-KISOTEN(IDX-Z)
                                               TO  WRK-HOUKNS-TENSU
                                                                 (IDX)
      *特異的ＩＧＥ
                   WHEN    11
                       MOVE    WRK-COM-HOUKNSKBN(IDX-Z)    TO  IDX
                       ADD     1               TO  WRK-HOUKNS-CNT (IDX)
                       ADD     WRK-COM-KISOTEN(IDX-Z)
                                               TO  WRK-HOUKNS-TENSU
                                                                 (IDX)
                   WHEN    OTHER
      *                逓減あり点数計算
                       IF      WRK-COM-TEIGENKBN (IDX-Z)   =   1
      *                    通則年齢対象外区分判定
      *********************IF      WRK-COM-TSUSOKUAGE (IDX-Z)  =   "1"
                           IF      WRK-COM-TUSOKUGAIKBN(IDX-Z) =   "0"
                               ADD     WRK-COM-KISOTEN (IDX-Z)
                                                   TO  WRK-TENSUKEI-TNEN
                           ELSE
                               ADD     WRK-COM-KISOTEN(IDX-Z)
                                                   TO  WRK-TENSUKEI-TEI
                           END-IF
                       ELSE
      *                    通則年齢対象外区分判定
      *********************IF      WRK-COM-TSUSOKUAGE (IDX-Z)  =   "1"
                           IF      WRK-COM-TUSOKUGAIKBN(IDX-Z) =   "0"
                               ADD     WRK-COM-KISOTEN (IDX-Z)
                                                   TO  WRK-TENSUKEI-NNEN
                           ELSE
                               ADD     WRK-COM-KISOTEN(IDX-Z)
                                                   TO  WRK-TENSUKEI-NASI
                           END-IF
                       END-IF
               END-EVALUATE
           END-IF
      *
           .
       1003-HOUKNS-HEN-EXT.
           EXIT.
      *----(01.00.01) LINE ADD START ----------------------------------
      *
      *****************************************************************
      *    包括・点数集計処理
      *****************************************************************
       1004-HOUKNS-SET-SEC                  SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEI
           MOVE    ZERO                TO  FLG-ERR
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   11  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               EVALUATE    IDX
                   WHEN    1
      *                血液化学検査の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    5   THRU  7
                               MOVE    "099600101"     TO  WRK-SRYCD
                           WHEN    8   THRU  9
                               MOVE    "099600102"     TO  WRK-SRYCD
                           WHEN    10  THRU  9999
                               MOVE    "099600103"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                           IF     (SPA-ERRCD       =   SPACE    )
                           AND    (WRK-HOUKNS-CNT(IDX) >=   10  )
                               MOVE    1           TO  WRK-COM-HOUKNS-10
                                                             (WRK-IDX)
                           END-IF
                       END-IF
                   WHEN    2
      *                内分泌学検査の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    3   THRU  5
                               MOVE    "099600201"     TO  WRK-SRYCD
                           WHEN    6   THRU  7
                               MOVE    "099600202"     TO  WRK-SRYCD
                           WHEN    8   THRU  9999
                               MOVE    "099600203"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    3
      *                肝炎ウイルス関連の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    3
                               MOVE    "099600301"     TO  WRK-SRYCD
                           WHEN    4
                               MOVE    "099600302"     TO  WRK-SRYCD
                           WHEN    5   THRU  9999
                               MOVE    "099600303"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    4
      *                腫瘍マーカーの包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    2   THRU  9999
                               MOVE    "099600401"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    5
      *                ４以外の腫瘍マーカーの包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    2
                               MOVE    "099600501"     TO  WRK-SRYCD
                           WHEN    3
                               MOVE    "099600502"     TO  WRK-SRYCD
                           WHEN    4   THRU  9999
                               MOVE    "099600503"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    6
      *                出血・凝固検査の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    3   THRU  4
                               MOVE    "099600601"     TO  WRK-SRYCD
                           WHEN    5   THRU  9999
                               MOVE    "099600602"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    7
      *                自己抗体検査の包括
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    2
                               MOVE    "099600701"     TO  WRK-SRYCD
                           WHEN    3   THRU  9999
                               MOVE    "099600702"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    9
      *            ウイルス抗体価
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    8   THRU  9999
                               MOVE    "099600901"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    10
      *            グロブリンクラス別ウイルス抗体価
                       MOVE    SPACE           TO  WRK-SRYCD
                       EVALUATE    WRK-HOUKNS-CNT(IDX)
                           WHEN    2   THRU  9999
                               MOVE    "099601001"     TO  WRK-SRYCD
                       END-EVALUATE
                       IF      WRK-SRYCD           =   SPACE
                           ADD     WRK-HOUKNS-TENSU (IDX)
                                                   TO  WRK-TENSUKEI
                       ELSE
                           PERFORM 10041-HOUKNS-ADD-SEC
                       END-IF
                   WHEN    11
      *            特異的ＩＧＥ
                       PERFORM 10042-IGE-HOUKNS-ADD-SEC
      *内分泌負荷試験は包括計算外
      *D           WHEN    8
      *                内部泌負荷試験の包括
      *                ADD     WRK-HOUKNS-TENSU (IDX)
      *D                                           TO  WRK-TENSUKEI
               END-EVALUATE
           END-PERFORM
      *
      *    手技料の合計点数をセット
           MOVE    WRK-TENSUKEI        TO  LNK-SC90-TENSU
      *
           .
       1004-HOUKNS-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括点数算定処理
      *****************************************************************
       10041-HOUKNS-ADD-SEC                  SECTION.
      *
           IF      FLG-ERR             =   1
               MOVE    "0028"              TO  SPA-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
               GO      TO      10041-HOUKNS-ADD-EXT
           END-IF
           MOVE    1                   TO  FLG-ERR
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   ZERO
               ADD     TNS-TEN             TO  WRK-TENSUKEI
           END-IF
      *
      *    検査の最終行判定
           MOVE    ZERO                TO  WRK-IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   IDX-MAX
               IF      WRK-COM-INPUTCD(IDY)(1:1)   =   "1"
                   MOVE    IDY                 TO  WRK-IDX
               END-IF
           END-PERFORM
           IF      WRK-IDX             =   ZERO
               MOVE    IDX-MAX             TO  WRK-IDX
           END-IF
      *
      *    包括の最後の行に包括数を編集する
           INITIALIZE                  ORCSMEIHENAREA
           MOVE    "2"                 TO  ORCSMEIHEN-KBN
           MOVE    WRK-HOUKNS-CNT(IDX) TO  ORCSMEIHEN-SURYO
           MOVE    WRK-GMN-MEISYO (WRK-IDX)
                                       TO  ORCSMEIHEN-TNS-NAME
           CALL    "ORCSMEIHEN"        USING
                                       SPA-AREA
                                       ORCSMEIHENAREA
           MOVE    ORCSMEIHEN-MEISYO   TO  WRK-GMN-MEISYO (WRK-IDX)
      *
      *    包括の最後の行に包括数を編集する
      *    MOVE    WRK-HOUKNS-CNT(IDX)     TO  WRK-Z9
      *    INITIALIZE                  ORCSKANACHKAREA
      *    MOVE    "2"                 TO  KANACHK-SYORI
      *    MOVE    WRK-Z9              TO  KANACHK-MAE-INPUT
      *    CALL    "ORCSKANACHK"       USING
      *                                ORCSKANACHKAREA
      *    IF      KANACHK-RC          =   ZERO
      *        MOVE    KANACHK-OUT-INPUT(1:4)
      *                                TO  WRK-HEN-Z9
      *    ELSE
      *        MOVE    SPACE           TO  WRK-HEN-Z9
      *    END-IF
      *****IF      WRK-GMN-INPUTCD(IDX-MAX)(1:1)   =   "*"
      *        MOVE    SPACE           TO  WRK-GMN-MEISYO (IDX-MAX)
      *****END-IF
      *    MOVE    WRK-GMN-MEISYO (WRK-IDX)    TO  WRK-MEISYO
      *    MOVE    SPACE               TO  WRK-GMN-MEISYO  (WRK-IDX)
      *    STRING  WRK-MEISYO          DELIMITED   BY  SPACE
      *            "　　項目数："      DELIMITED   BY  SIZE
      *            WRK-HEN-Z9          DELIMITED   BY  SIZE
      *            INTO                    WRK-GMN-MEISYO (WRK-IDX)
      *    END-STRING
      *
           .
       10041-HOUKNS-ADD-EXT.
           EXIT.
      *****************************************************************
      *    特異的ＩＧＥ算定処理
      *****************************************************************
       10042-IGE-HOUKNS-ADD-SEC                  SECTION.
      *
           MOVE    "099601101"         TO  WRK-SRYCD
      *
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           ADD     WRK-HOUKNS-TENSU (IDX)  TO  WRK-TENSUKEI
           IF      WRK-TENSUKEI            >   TNS-TEN
               MOVE    TNS-TEN             TO  WRK-TENSUKEI
           END-IF
      *
      *    検査の最終行判定
           MOVE    ZERO                TO  WRK-IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   IDX-MAX
               IF      WRK-COM-INPUTCD(IDY)(1:1)   =   "1"
                   MOVE    IDY                 TO  WRK-IDX
               END-IF
           END-PERFORM
           IF      WRK-IDX             =   ZERO
               MOVE    IDX-MAX             TO  WRK-IDX
           END-IF
      *
           .
       10042-IGE-HOUKNS-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    割合加算・減算計算処理
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX-SYU
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           MOVE    ZERO                TO  WRK-TENSUKEI-K
      *    年齢以外の加算
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE   )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE  AND
                               WRK-COM-INPUTCD (IDX)   =   SPACE )
      *        包括以外の加算で１剤につき１手技である
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-DATAKBN(IDX)        =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   3   )
                   MOVE    IDX                 TO  IDX-SYU
               END-IF
      *
      *        加算（通則年齢以外）
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-DATAKBN    (IDX)    =   "2" )  AND
                      (WRK-COM-TSUSOKUAGE (IDX)    NOT =   "1" )  AND
                      (IDX-SYU                     >   ZERO)
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                              (IDX-SYU)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-K
               END-IF
      *        減算
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   6   )  AND
                      (IDX-SYU                     >   ZERO)
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   IF      WRK-COM-KIJUNTEIGENKBN (IDX)  NOT =   ZERO
      *                基準不適合逓減の時、逓減分より逓減する
                       MOVE    ZERO            TO  IDX3
                       PERFORM VARYING     IDX2    FROM    1  BY  1
                               UNTIL       IDX2    >=  IDX
                           IF      WRK-COM-KIJUNTEIGENKBN (IDX2)
                                                       =   2  OR  3
                               MOVE    IDX2            TO  IDX3
                               MOVE    IDX             TO  IDX2
                           END-IF
                       END-PERFORM
                       IF      IDX3            >   ZERO
                           COMPUTE WRK-TENSUKEI    =   WRK-COM-KISOTEN
                                                              (IDX3)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                       END-IF
                   END-IF
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
      *    IF      IDX-SYU             =   ZERO
      *        GO      TO      1006-KASAN-SET-EXT
      *    END-IF
      *
      *    検査逓減減算
           IF     (WRK-TENSUKEI-TEI    >   ZERO )  OR
                  (WRK-TENSUKEI-TNEN   >   ZERO )
      *        手技料が通則年齢加算あり
               IF      WRK-COM-TSUSOKUAGE(IDX-SYU) =   "1"
                   COMPUTE WRK-TENSUKEI-TNEN   =   WRK-TENSUKEI-TNEN
                                               +   WRK-TENSUKEI-K
                                               -   WRK-TENSUKEI-G
               ELSE
                   COMPUTE WRK-TENSUKEI-TEI    =   WRK-TENSUKEI-TEI
                                               +   WRK-TENSUKEI-K
                                               -   WRK-TENSUKEI-G
               END-IF
      *
               MOVE    ZERO                TO  WRK-TENSUKEI-G1
               MOVE    ZERO                TO  WRK-TENSUKEI-G2
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   SPA-MAX-LINE )   OR
                                  (WRK-GMN-INPUTCD (IDX)   =   SPACE AND
                                   WRK-COM-INPUTCD (IDX)   =   SPACE )
                   IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                          (WRK-COM-TENSIKIBETU(IDX)    =   6   )  AND
                          (WRK-COM-KIJUNTEIGENKBN(IDX) =   ZERO )
                       MOVE    ZERO                TO  WRK-TENSUKEI
      *                検査逓減の時、逓減分の合計より逓減する
                       COMPUTE WRK-TENSUKEI    =   WRK-TENSUKEI-TEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                       MOVE    WRK-TENSUKEI    TO  WRK-TENSUKEI-G1
                       COMPUTE WRK-TENSUKEI    =   WRK-TENSUKEI-TNEN
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                       MOVE    WRK-TENSUKEI    TO  WRK-TENSUKEI-G2
                   END-IF
               END-PERFORM
      *
               COMPUTE WRK-TENSUKEI-TEI        =   WRK-TENSUKEI-TEI
                                               -   WRK-TENSUKEI-G1
               COMPUTE WRK-TENSUKEI-TNEN       =   WRK-TENSUKEI-TNEN
                                               -   WRK-TENSUKEI-G2
      *
           ELSE
      *        検査逓減減算以外
      *        手技料が通則年齢加算あり
               IF      WRK-COM-TSUSOKUAGE(IDX-SYU) =   "1"
                   COMPUTE WRK-TENSUKEI-NNEN   =   WRK-TENSUKEI-NNEN
                                               +   WRK-TENSUKEI-K
                                               -   WRK-TENSUKEI-G
               ELSE
                   COMPUTE WRK-TENSUKEI-NASI   =   WRK-TENSUKEI-NASI
                                               +   WRK-TENSUKEI-K
                                               -   WRK-TENSUKEI-G
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  WRK-TENSUKEI-K1
           MOVE    ZERO                TO  WRK-TENSUKEI-K2
      *    年齢加算計算
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE   )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE  AND
                               WRK-COM-INPUTCD (IDX)   =   SPACE )
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-DATAKBN    (IDX)    =   "2" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-TSUSOKUAGE (IDX)    =   "1" )
      *            逓減あり
                   COMPUTE WRK-TENSUKEI        =   WRK-TENSUKEI-TNEN
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-K1
      *            逓減なし
                   COMPUTE WRK-TENSUKEI        =   WRK-TENSUKEI-NNEN
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-K2
               END-IF
           END-PERFORM
      *    逓減ありの点数
           COMPUTE WRK-TENSUKEI-TNEN   =   WRK-TENSUKEI-TNEN
                                       +   WRK-TENSUKEI-K1
      *    逓減なしの点数
           COMPUTE WRK-TENSUKEI-NNEN   =   WRK-TENSUKEI-NNEN
                                       +   WRK-TENSUKEI-K2
      *
           COMPUTE LNK-SC90-TENSU      =   LNK-SC90-TENSU
                                       +   WRK-TENSUKEI-TEI
                                       +   WRK-TENSUKEI-TNEN
                                       +   WRK-TENSUKEI-NASI
                                       +   WRK-TENSUKEI-NNEN
      *
           IF      SPA-HKNKBN          =   1
             AND  (IDX-SYU         NOT =   ZERO)
      ****   AND  (SPA-RSI-HKNKBN  NOT =   "5" )
             AND  (SPA-RSI-JUNKYO  NOT =   "1" )
      *        労災の外来管理加算読み替え処理
               PERFORM 10061-RSI-SYORI-SEC
           END-IF
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の外来管理加算読み替え処理
      *****************************************************************
       10061-RSI-SYORI-SEC             SECTION.
      *
      *    外来管理加算の算定できない手技を検索
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE   )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE  AND
                               WRK-COM-INPUTCD (IDX)   =   SPACE )
               IF      (WRK-COM-INPUTCD(IDX)(1:1)      =   "1" )  AND
                       (WRK-COM-INPUTCD(IDX)(1:3)  NOT =   "101") AND
                       (WRK-COM-GAIKANRIKBN (IDX)      =   1   )
      *            手技料の合計を編集する
                   MOVE    LNK-SC90-TENSU      TO  WRK-COM-SYUTEN2
                                                            (IDX)
      *            労災の外来管理加算読み替え処理
                   IF     (WRK-COM-RSIGIRI (IDX)
                                            =   1  )  AND
                          (WRK-COM-SYUTEN2 (IDX)
                                            <   LNK-ORCSC-GAIRAI-TEN)
                       MOVE     LNK-ORCSC-GAIRAI-TEN
                                               TO  LNK-SC90-TENSU
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       10061-RSI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    全診療行為チェック処理
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
      *    包括まとめ入力は以下対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               CONTINUE
           ELSE
      *        包括同一検査チェック
               IF     (SPA-NYUGAIKBN       =   "2" )  OR
                      (LNK-ORCSC-KENSAFLG  =   1   )
                   IF      LNK-ORCSC-PG    NOT =   "KT01"
                       PERFORM 2001-DOUCHK-SEC
                   END-IF
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   IF      SPA-NYUGAIKBN       =   "1"
      *            入院時のチェック
                       PERFORM 210-NYUIN-SYORI-SEC
                   ELSE
      *            外来時のチェック
                       PERFORM 200-GAIRAI-SYORI-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     同一検査チェック処理
      *****************************************************************
       2001-DOUCHK-SEC           SECTION.
      *
      *    同一検査をエラーとする
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE  AND
                               WRK-COM-INPUTCD (IDX-Z) =   SPACE )
      *
             IF      WRK-COM-HKNCOMBI(IDX-Z) NOT =   "9999"
      *
               IF     (WRK-COM-SRYKBN (IDX-Z)  =   "60"  )  AND
                      (WRK-COM-INPUTCD(IDX-Z)(1:1)  =   "1" )  AND
                      (WRK-COM-DATAKBN(IDX-Z)  =   "1"   )
      *        逓減以外
               AND    (WRK-COM-TEIGENKBN (IDX-Z)   NOT =   1    )
      *        包括対象以外
               AND    (WRK-COM-SRYSYUKBN (IDX-Z)   NOT =   "610")
      *        検体検査コメントの判定
               AND    (WRK-COM-KENTAICOMMENT(IDX-Z)    =   ZERO  )
                   PERFORM VARYING     IDX-Z2  FROM    1   BY  1
                           UNTIL      (IDX-Z2      >   SPA-MAX-LINE) OR
                                      (SPA-ERRCD   NOT =   SPACE )  OR
                                  (WRK-GMN-INPUTCD (IDX-Z2) =   SPACE )
      *
                     IF      WRK-COM-HKNCOMBI(IDX-Z2) NOT =   "9999"
      *
                       IF     (IDX-Z2      NOT =   IDX-Z )  AND
                              (WRK-COM-SRYSYUKBN   (IDX-Z2)
                                           NOT =   "610"  )  AND
      *                        労災の検査は対象外
                              (WRK-COM-INPUTCD(IDX-Z2)(1:3)
                                           NOT =   "101" )  AND
                              (WRK-COM-INPUTCD(IDX-Z)
                                               =   WRK-COM-INPUTCD
                                                        (IDX-Z2))
      *                 検体検査コメントの判定
                         AND  (WRK-COM-KENGRP(IDX-Z)
                                             =   WRK-COM-KENGRP(IDX-Z2))
                           IF      WRK-COM-HOUKNSKBN(IDX-Z)  =   ZERO
                               MOVE    "0064"          TO  SPA-ERRCD
                               MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z2)
                           ELSE
      *                        包括の時、削除確認メッセージ
                               MOVE    "0160"      TO  LNK-ORCSC-KIDCD
                               MOVE    "0064"          TO  SPA-ERRCD
                               MOVE    "1"             TO  WRK-COM-CUR
                                                             (IDX-Z2)
                           END-IF
                       END-IF
                     END-IF
                   END-PERFORM
               END-IF
            END-IF
           END-PERFORM
      *
           .
       2001-DOUCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     逓減加算算定チェック処理
      *****************************************************************
       200-GAIRAI-SYORI-SEC           SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-JIKASAN
           MOVE    ZERO                TO  IDX-JIKAN
           MOVE    ZERO                TO  IDX-TEIGEN
           MOVE    ZERO                TO  IDX-TEIGEN2
           INITIALIZE                      TBL-TEIGEN-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE  AND
                               WRK-COM-INPUTCD (IDX-Z) =   SPACE )
      *
            IF      WRK-COM-HKNCOMBI(IDX-Z) NOT =   "9999"
      *
      *        時間外緊急院内画像診断加算
               IF      WRK-COM-INPUTCD(IDX-Z)  =   "160000210"
                   MOVE    IDX-Z           TO  IDX-JIKAN
               END-IF
      *
      *        病理診断料チェック
               IF      WRK-COM-INPUTCD (IDX-Z) =   "160155110"
                   PERFORM 1006-SINDAN-CHK-SEC
               END-IF
      *
      *        病理学的検査診断料チェック
               IF      WRK-COM-INPUTCD (IDX-Z) =   "160062310"
                   PERFORM 1007-KENSA-CHK-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-IF
      *
      *        時間外加算チェック（診察、加算、時間加算のもの）
               IF     (WRK-COM-SRYKBN  (IDX-Z)     =   "11"  OR
                                                       "12"     )
                   IF     (WRK-COM-DATAKBN   (IDX-Z)   =   "2"  ) AND
                          (WRK-COM-TIMEKSNKBN(IDX-Z)   NOT =   ZERO)
                       MOVE    1               TO  FLG-JIKASAN
                   END-IF
                   IF     (WRK-COM-DATAKBN (IDX-Z)     =   "1"    ) AND
                          (WRK-COM-TIMEFLG (IDX-Z)     NOT =   ZERO )
                       MOVE    1               TO  FLG-JIKASAN
                   END-IF
               END-IF
      ***      逓減加算算定チェック
      *!       IF     (WRK-COM-TEIGENKBN (IDX-Z)   =   1    )  AND
      *!              (WRK-COM-DATAKBN   (IDX-Z)   =  "1"   )  AND
      *!              (WRK-COM-TEIGEN-FLG(IDX-Z)   =   ZERO )
      *!           PERFORM 2008-TEIGEN-INSYORI-SEC
      *!       END-IF
      *!       IF     (WRK-COM-TEIGENKBN (IDX-Z)   =   1    )  AND
      *!              (WRK-COM-DATAKBN   (IDX-Z)   =  "1"   )
      *!           MOVE    IDX-Z               TO  IDX-TEIGEN2
      ****     END-IF
      *        逓減加算算定チェック
      *        IF     (WRK-COM-SRYKBN    (IDX-Z)   =   "60" )  AND
      *               (WRK-COM-TEIGENKBN (IDX-Z)   =   1    )  AND
      *               (WRK-COM-DATAKBN   (IDX-Z)   =  "1"   )
      *            回数を入院回数へ編集する
      *            MOVE    WRK-COM-KAISU (IDX-Z)   TO  WRK-COM-NYU-KAI
      *                                                       (IDX-Z)
      *            IF      WRK-COM-TEIGEN-FLG(IDX-Z)   =  2
      *                MOVE    ZERO        TO  WRK-COM-TEIGEN-FLG(IDX-Z)
      *            END-IF
      *            PERFORM 20018-TEIGEN-INSYORI-SEC
      *****    END-IF
      *
      *        重複算定不可チェック
               IF     (WRK-COM-SRYKBN    (IDX-Z)   =   "60"  )  AND
                      (WRK-COM-DATAKBN   (IDX-Z)   =   "1"   )  AND
                      (WRK-COM-TEIGEN-FLG(IDX-Z)   =   ZERO )
                   PERFORM 20011-JYUFUKU-CHK-SEC
      *            ２つ以上のチェック
                   PERFORM 20012-JYUFUKU-CHK2-SEC
               END-IF
      *!
      *        コンタクトレンズ検査料の算定不可
               IF     (SPA-ERRCD           =   SPACE )  AND
                      (WRK-COM-CDKBN-KBN   (IDX-Z)
                                           =   "D"   )  AND
                      (WRK-COM-CDKBN-KBNNUM (IDX-Z)
                                           =   "282" )  AND
                      (WRK-COM-CDKBN-KBNNUM-EDA(IDX-Z)
                                           =   "03"   )
                   PERFORM 2003-CCONTAKU-CHK2-SEC
               END-IF
             END-IF
           END-PERFORM
      *
      *    逓減加算算定あり
      *!   IF      IDX-TEIGEN          >   ZERO
      *!       PERFORM 2009-TEIGEN-TUIKA-SEC
      *!   END-IF
      *
      *    時間外緊急院内画像診断加算有り
           IF      IDX-JIKAN           >   ZERO
               MOVE    IDX-JIKAN           TO  IDX-Z
               IF     (FLG-JIKASAN         =   ZERO         )  AND
                      (WRK-COM-KZMFLG (IDX-Z)  =   SPACE    )
                   MOVE    "K001"              TO  SPA-ERRCD
                   MOVE    "2"                 TO  WRK-COM-CUR (IDX-Z)
                   MOVE    "1"                 TO  WRK-COM-KZMFLG
                                                               (IDX-Z)
                   GO      TO      200-GAIRAI-SYORI-EXT
               END-IF
      *        時間入力追加
               MOVE    "840000044"         TO  WRK-SRYCD
               IF      WRK-COM-INPUTCD(IDX-Z + 1)  =   WRK-SRYCD
                                                   OR  CONS-JIKAN
                   CONTINUE
               ELSE
                   MOVE    IDX-Z               TO  IDX-S
                   MOVE    ZERO                TO  FLG-KASAN
                   PERFORM 2001-JIDOTUIKA-SEC
                   IF      FLG-ARI             =   1
                       MOVE    "2"             TO  WRK-COM-AUTOKBN
                                                              (IDX-T)
                       MOVE    "1"             TO  WRK-COM-IN  (IDX-T)
                       MOVE    "2"             TO  WRK-COM-IN2 (IDX-T)
                   END-IF
               END-IF
           END-IF
      *
           .
       200-GAIRAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     逓減加算算定チェック処理
      *****************************************************************
       2008-TEIGEN-INSYORI-SEC           SECTION.
      *
      *    同一検査算定有無の判定
           SET     KEN-IDX             TO  1
           SEARCH      TBL-KENSA-HOU-OCC   VARYING   KEN-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-KEN-HOU
                   WHEN    WRK-COM-INPUTCD (IDX-Z)
                                               =   TBL-KEN-SRYCD
                                                           (KEN-IDX)
                       MOVE    TBL-KEN-HOU  (KEN-IDX)
                                                TO  WRK-KEN-HOU
           END-SEARCH
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   KEN-IDX-MAX)  OR
                              (WRK-KEN-HOU     =   SPACE  )  OR
                              (SPA-ERRCD   NOT =   SPACE  )  OR
                              (WRK-SANTEI-CNT  >   ZERO   )
               SET     KEN-IDX             TO  IDX
               IF     (TBL-KEN-HOU  (KEN-IDX)  =   WRK-KEN-HOU ) AND
                      (TBL-KEN-SRYCD(KEN-IDX)
                                           NOT =   WRK-COM-INPUTCD
                                                         (IDX-Z))
                   MOVE    TBL-KEN-SRYCD (KEN-IDX) TO  WRK-SRYCD
                   PERFORM 20081-SANTEI-TBLCHK-SEC
               END-IF
           END-PERFORM
      *
      *    入院の時、複数日で逓減なしの時エラー
      *    IF     (SPA-NYUGAIKBN       =   "1"  )  AND
      *           (WRK-SANTEI-CNT      >=  WRK-COM-NYU-KAI (IDX-Z) )
      *        IF      WRK-COM-NYU-KAI (IDX-Z)     >   1
      *            MOVE    "9021"              TO  SPA-ERRCD
      *            MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
      *        END-IF
      *    END-IF
      *
      *    逓減加算自動追加
           IF      WRK-SANTEI-CNT      >   ZERO
               MOVE    IDX-Z               TO  IDX-TEIGEN
           END-IF
           .
      *
       2008-TEIGEN-INSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    逓減加算算定入力テーブル追加処理
      *****************************************************************
       2009-TEIGEN-TUIKA-SEC                  SECTION.
      *
      **** MOVE    1                   TO  WRK-COM-TEIGEN-FLG
           MOVE    2                   TO  WRK-COM-TEIGEN-FLG
                                                        (IDX-TEIGEN)
           MOVE    "160000190"         TO  WRK-SRYCD
           MOVE    IDX-TEIGEN2         TO  IDX-S
           MOVE    2                   TO  FLG-KASAN
           PERFORM 2001-JIDOTUIKA-SEC
           MOVE    "2"                 TO  WRK-COM-AUTOKBN(IDX-T)
      *
           .
       2009-TEIGEN-TUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    逓減加算算定入力テーブルチェック処理
      *****************************************************************
       20081-SANTEI-TBLCHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >=  IDX-Z      )  OR
                              (WRK-SANTEI-CNT  >   ZERO   )
               IF      WRK-SRYCD           =   WRK-COM-INPUTCD
                                                         (IDX2)
                   IF      WRK-COM-NYU-KAI (IDX-Z)     >   1
                       ADD     WRK-COM-NYU-KAI (IDX-Z)
                                               TO  WRK-SANTEI-CNT
                   ELSE
                       ADD     1               TO  WRK-SANTEI-CNT
                   END-IF
               END-IF
           END-PERFORM
           .
      *
       20081-SANTEI-TBLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病理診断料チェック処理
      *****************************************************************
       1006-SINDAN-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
      *T   同月内の検査区分番号のチェック
      *    PERFORM VARYING IDX-ACT     FROM    1   BY  1
      *            UNTIL  (IDX-ACT     >   100  )  OR
      *                   (LNK-ACCT-SRYCD (IDX-ACT)    =   SPACE )
      *        IF      LNK-ACCT-CDKBN (IDX-ACT)    =   "D100" OR
      *                                                "D101"
      *            MOVE    1               TO  FLG-OK
      *        END-IF
      *T   END-PERFORM
      *
      *    今回入力の検査区分番号のチェック
           PERFORM VARYING IDX-ACT     FROM    1   BY  1
                   UNTIL  (IDX-ACT     >   SPA-MAX-LINE  )  OR
                         ((WRK-GMN-INPUTCD (IDX-ACT)  =   SPACE )  AND
                          (WRK-COM-INPUTCD (IDX-ACT)  =   SPACE ))
               IF     (IDX-ACT         NOT =   IDX-Z  )  AND
                      (WRK-COM-CDKBN-KBN   (IDX-ACT)
                                           =   "N"   )  AND
                      (WRK-COM-CDKBN-KBNNUM (IDX-ACT)
                                           =   "000"
                                           OR  "001"
                                           OR  "002"
                                           OR  "003"  )
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
      *    同月内の検査区分番号のチェック
           IF      FLG-OK              =   ZERO
               IF      LNK-ACCT-SRYCD (01) =   SPACE
                   PERFORM 10060-SINDAN-ACCT-HEN-SEC
               END-IF
               PERFORM VARYING IDX-ACT     FROM    1   BY  1
                       UNTIL  (IDX-ACT     >   100  )  OR
                              (LNK-ACCT-SRYCD (IDX-ACT)    =   SPACE )
                   IF      LNK-ACCT-CDKBN (IDX-ACT)    =   "N000"
                                                       OR  "N001"
                                                       OR  "N002"
                                                       OR  "N003"
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
           END-IF
      *!!!
      *    外来まとめのチェックなし
           IF     (LNK-ORCSC-PG        =   "KT01" )
             OR   (LNK-ORCSC-KT01FLG   =   "1"    )
               MOVE    1                   TO  FLG-OK
           END-IF
      *!!
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0026"          TO  SPA-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
           END-IF
      *
      *    病理学的検査診断料の算定チェック
           INITIALIZE                      ORCSC91AREA
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
           MOVE    "2"                 TO  LNK-SC91-KBN
           MOVE    "160062310"         TO  LNK-SC91-SRYCD
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC91-TEISEI-AREA
           MOVE    LNK-ORCSC-KT01-AREA
                                       TO  LNK-SC91-KT01-AREA
           CALL    "ORCSC91"           USING
                                       MCPAREA
                                       ORCSC91AREA
                                       WRK-SCR-REC
                                       SPA-AREA
                                       TNS-TENSU-REC
      *
           IF      LNK-SC91-CNT        >   ZERO
      *        算定あり
               MOVE    "0027"              TO  SPA-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
           END-IF
           .
       1006-SINDAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病理診断料チェック処理
      *****************************************************************
       1007-KENSA-CHK-SEC                  SECTION.
      *
      *    病理学的診断料の算定チェック
           INITIALIZE                      ORCSC91AREA
           MOVE    "2"                 TO  LNK-SC91-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
           MOVE    "160155110"         TO  LNK-SC91-SRYCD
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC91-TEISEI-AREA
           MOVE    LNK-ORCSC-KT01-AREA
                                       TO  LNK-SC91-KT01-AREA
           CALL    "ORCSC91"           USING
                                       MCPAREA
                                       ORCSC91AREA
                                       WRK-SCR-REC
                                       SPA-AREA
                                       TNS-TENSU-REC
      *
           IF      LNK-SC91-CNT        >   ZERO
      *        算定あり
               MOVE    "0018"              TO  SPA-ERRCD
      ******** MOVE    "1"                 TO  WRK-COM-CUR (IDX-Z)
           END-IF
           .
       1007-SINDAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     同月内の検査区分番号のチェック処理
      *****************************************************************
       10060-SINDAN-ACCT-HEN-SEC           SECTION.
      *
           INITIALIZE                  LNK-NAIACCT-AREA
           MOVE    ZERO                TO  IDX-ACT
      *
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    "60"                TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM UNTIL       (FLG-SRYACCT         =   1       )
      *
               IF     (ACCT-HKNCOMBI     =   "9999" ) OR
                      (ACCT-ZAIKAISU     =   ZERO   )
                   CONTINUE
               ELSE
      *            診療行為マスター編集
                   PERFORM 100601-SRYACT-HEN-SEC
               END-IF
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX-ACT             =   ZERO
               MOVE    "999999999"     TO  LNK-ACCT-SRYCD (1)
           END-IF
      *
           .
       10060-SINDAN-ACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病理診断算定編集処理
      *****************************************************************
       100601-SRYACT-HEN-SEC            SECTION.
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
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
                   IF      SRY-SRYCD (IDX2)(1:1)   =   "1"
      *                点数マスタより、区分番号編集
                       MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF     (TNS-DATAKBN         =   "1" )  AND
                              (TNS-CDKBN-KBN       =   "N" )  AND
                              (TNS-CDKBN-KBNNUM    =   "000"
                                                   OR  "001"
                                                   OR  "002"
                                                   OR  "003" )
                           ADD     1               TO  IDX-ACT
                           IF      IDX-ACT         >   100
                               MOVE    100         TO  IDX-ACT
                           END-IF
                           MOVE    ACCT-SRYKBN     TO  LNK-ACCT-SRYKBN
                                                      (IDX-ACT)
                           MOVE    SRY-SRYCD (IDX2)
                                                   TO  LNK-ACCT-SRYCD
                                                           (IDX-ACT)
                           MOVE    TNS-CDKBN-KBN
                                               TO  LNK-ACCT-CDKBN-KBN
                                                          (IDX-ACT)
                           MOVE    TNS-CDKBN-KBNNUM
                                               TO  LNK-ACCT-CDKBN-KBNNUM
                                                          (IDX-ACT)
                       END-IF
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
       100601-SRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     重複算定不可チェック処理
      *****************************************************************
       20011-JYUFUKU-CHK-SEC           SECTION.
      *
      *    同一検査算定有無の判定
           SET     JYU-IDX             TO  1
           SEARCH      TBL-KENSA-JYU-OCC   VARYING   JYU-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-JYU-KBN
                       MOVE    ZERO            TO  WRK-JYU-KEN
                       MOVE    SPACE           TO  WRK-JYU-FLG
                   WHEN    WRK-COM-INPUTCD (IDX-Z)
                                               =   TBL-JYU-SRYCD
                                                           (JYU-IDX)
                       MOVE    TBL-JYU-KBN  (JYU-IDX)
                                                TO  WRK-JYU-KBN
                       MOVE    TBL-JYU-KEN  (JYU-IDX)
                                                TO  WRK-JYU-KEN
                       MOVE    TBL-FILLER2  (JYU-IDX)
                                                TO  WRK-JYU-FLG
           END-SEARCH
      *
           IF      WRK-JYU-KBN         =   SPACE
               GO      TO      20011-JYUFUKU-CHK-EXT
           END-IF
      *    包括対象以外
           IF     (WRK-COM-SRYSYUKBN(IDX-Z)    =   "610" )  OR
                 ((SPA-NYUGAIKBN           =   "1"  )  AND
                  (LNK-ORCSC-KENSAFLG      =   ZERO ))
               MOVE    1               TO  FLG-HOUKATU
               MOVE    IDX-Z           TO  IDX-STR
           ELSE
               MOVE    ZERO            TO  FLG-HOUKATU
               MOVE    1               TO  IDX-STR
           END-IF
      *
           MOVE    ZERO                TO  WRK-JYU-CNT
           MOVE    WRK-COM-NAME (IDX-Z)    TO  WRK-ERRMSG-1
           MOVE    SPACE               TO  WRK-ERRMSG-2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   KEN-JYU-MAX)  OR
                              (SPA-ERRCD   NOT =   SPACE  )
               SET     JYU-IDX             TO  IDX
               IF     (TBL-JYU-KBN  (JYU-IDX)  =   WRK-JYU-KBN ) AND
                      (TBL-JYU-SRYCD(JYU-IDX)
                                           NOT =   WRK-COM-INPUTCD
                                                         (IDX-Z))
                   MOVE    TBL-JYU-SRYCD (JYU-IDX) TO  WRK-SRYCD
                   PERFORM VARYING     IDX2    FROM    IDX-STR  BY  1
                           UNTIL      (IDX2    >   SPA-MAX-LINE )  OR
                                      (WRK-COM-INPUTCD (IDX2)
                                                   =   SPACE  AND
                                       WRK-GMN-INPUTCD (IDX2)
                                                   =   SPACE  )
      *
                     IF      WRK-COM-HKNCOMBI(IDX2) NOT =   "9999"
      *
                       IF     (WRK-COM-INPUTCD(IDX-Z)
                                               NOT =   WRK-COM-INPUTCD
                                                             (IDX2)) AND
                              (WRK-COM-INPUTCD (IDX2)  =   WRK-SRYCD)
                          AND (WRK-COM-SRYSYUKBN(IDX2) =
                                               WRK-COM-SRYSYUKBN(IDX-Z))
                          IF      SPA-NYUGAIKBN   =   "1"
                              PERFORM 200111-JYUFUKU-NYU-SEC
                           ELSE
                               ADD     1               TO  WRK-JYU-CNT
                               MOVE    IDX2            TO  WRK-JYU-IDX
                               PERFORM 200111-ERRMSG-HEN-SEC
                           END-IF
                       END-IF
      *                包括・入院の時は剤内
                       IF     (FLG-HOUKATU             =   1  )  AND
                              (WRK-COM-ZAIEND(IDX2)    =   "1" )
                           MOVE    SPA-MAX-LINE        TO  IDX2
                       END-IF
                     END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           IF      WRK-JYU-KEN         <=  WRK-JYU-CNT
               MOVE    "0601"              TO  SPA-ERRCD
               IF      WRK-JYU-KEN         =   2
                   MOVE    "0602"              TO  SPA-ERRCD
               END-IF 
               MOVE    "1"                 TO  WRK-COM-CUR (WRK-JYU-IDX)
               MOVE    WRK-ERRMSG-1        TO  LNK-ORCSC-ERRMSG
           END-IF
      *
      **** IF      WRK-JYU-FLG     NOT =   SPACE
      *        別テーブルのチェック
      *        PERFORM 20012-JYUFUKU-CHK2-SEC
      *****END-IF
           .
      *
       20011-JYUFUKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    重複エラーメッセージ編集処理
      *****************************************************************
       200111-ERRMSG-HEN-SEC           SECTION.
      *
           MOVE    WRK-ERRMSG-1        TO  WRK-ERRMSG-2
           MOVE    SPACE               TO  WRK-ERRMSG-1
           IF      WRK-ERRMSG-2        =   SPACE
               MOVE    WRK-COM-NAME (IDX2) TO  WRK-ERRMSG-1
           ELSE
               STRING  WRK-ERRMSG-2        DELIMITED  BY  SPACE
                       "，"                DELIMITED  BY  SIZE
                       WRK-COM-NAME (IDX2)
                                           DELIMITED  BY  SPACE
                                       INTO  WRK-ERRMSG-1
               END-STRING
           END-IF
           .
      *
       200111-ERRMSG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     コンタクトレンズ検査料の算定不可
      *****************************************************************
       2003-CCONTAKU-CHK2-SEC           SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   SPA-MAX-LINE  )  OR
                              (WRK-COM-INPUTCD (IDX2)  =   SPACE  AND
                               WRK-GMN-INPUTCD (IDX2)  =   SPACE )  OR
                              (SPA-ERRCD   NOT =   SPACE  )
               IF     (IDX-Z           NOT =   IDX2   )  AND
                      (WRK-COM-CDKBN-KBN(IDX2)
                                           =   "D"   )
                   EVALUATE    WRK-COM-CDKBN-KBNNUM (IDX2)
                       WHEN    "255"  THRU   "281"
                           MOVE    "0600"          TO  SPA-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                                  (IDX2)
                       WHEN    "282"
                           IF      WRK-COM-CDKBN-KBNNUM-EDA(IDX2)
                                                   =   "00"
                                                   OR  "02"
                                                   OR  "03"
                               MOVE    "0600"          TO  SPA-ERRCD
                               MOVE    "1"             TO  WRK-COM-CUR
                                                                  (IDX2)
                           END-IF
                   END-EVALUATE
               END-IF
           END-PERFORM
           .
       2003-CCONTAKU-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *     重複算定不可チェック処理（入院日）
      *****************************************************************
       200111-JYUFUKU-NYU-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUDAY
           PERFORM VARYING     IDX-N2  FROM    1   BY  1
                    UNTIL      IDX-N2  >   31
               IF     (WRK-COM-NYUINDAY  (IDX-Z IDX-N2)
                                           NOT =   ZERO )  AND
                      (WRK-COM-NYUINDAY  (IDX2 IDX-N2)
                                           NOT =   ZERO )
                   MOVE    1               TO  FLG-NYUDAY
                   MOVE    31              TO  IDX-N2
               END-IF
           END-PERFORM
           IF      FLG-NYUDAY          =   1
               ADD     1               TO  WRK-JYU-CNT
               MOVE    IDX2            TO  WRK-JYU-IDX
               PERFORM 200111-ERRMSG-HEN-SEC
           END-IF
           .
       200111-JYUFUKU-NYU-EXT.
           EXIT.
      *
      *****************************************************************
      *     重複算定不可チェック２処理
      *****************************************************************
       20012-JYUFUKU-CHK2-SEC           SECTION.
      *
      *    同一検査算定有無の判定
           SET     JYU2-IDX            TO  1
           SEARCH      TBL-KENSA-JYU2-OCC   VARYING   JYU2-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-JYU-KBN
                       MOVE    ZERO            TO  WRK-JYU-KEN
                   WHEN    WRK-COM-INPUTCD (IDX-Z)
                                               =   TBL2-JYU-SRYCD
                                                           (JYU2-IDX)
                       MOVE    TBL2-JYU-KBN  (JYU2-IDX)
                                                TO  WRK-JYU-KBN
                       MOVE    TBL2-JYU-KEN  (JYU2-IDX)
                                                TO  WRK-JYU-KEN
           END-SEARCH
      *
           IF      WRK-JYU-KBN         =   SPACE
               GO      TO      20012-JYUFUKU-CHK2-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-JYU-CNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   KEN-JYU2-MAX)  OR
                              (SPA-ERRCD   NOT =   SPACE  )
               SET     JYU2-IDX            TO  IDX
               IF     (TBL2-JYU-KBN  (JYU2-IDX)  =   WRK-JYU-KBN ) AND
                      (TBL2-JYU-SRYCD(JYU2-IDX)
                                           NOT =   WRK-COM-INPUTCD
                                                         (IDX-Z))
                   MOVE    TBL2-JYU-SRYCD (JYU2-IDX) TO  WRK-SRYCD
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL      (IDX2    >   SPA-MAX-LINE  )  OR
                                      (WRK-COM-INPUTCD (IDX2)
                                                   =   SPACE  AND
                                       WRK-GMN-INPUTCD (IDX2)
                                                   =   SPACE  )
      *
                     IF      WRK-COM-HKNCOMBI(IDX2) NOT =   "9999"
      *
                       IF     (WRK-COM-INPUTCD(IDX-Z)
                                               NOT =   WRK-COM-INPUTCD
                                                             (IDX2)) AND
                              (WRK-COM-INPUTCD (IDX2)  =   WRK-SRYCD)
      *                   包括対象以外
                         AND  (WRK-COM-SRYSYUKBN(IDX2) NOT =   "610")
                           ADD     1               TO  WRK-JYU-CNT
                           MOVE    IDX2            TO  WRK-JYU-IDX
                       END-IF
                     END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           IF      WRK-JYU-KEN         <=  WRK-JYU-CNT
               MOVE    "0060"              TO  SPA-ERRCD
               MOVE    "1"                 TO  WRK-COM-CUR (WRK-JYU-IDX)
           END-IF
           .
      *
       20012-JYUFUKU-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算　存在チェック加処理
      *****************************************************************
       1110-KASAN-CHK-SEC        SECTION.
      *
      *    自動加算　存在チェック
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
      *****PERFORM VARYING     IDX1    FROM    IDX-Z  BY  1
           PERFORM VARYING     IDX1    FROM    IDX-S  BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     ) OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-COM-INPUTCD (IDX1)  =   SPACE  AND
                               WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                    MOVE    IDX1           TO  IDX-T
                    MOVE    1              TO  FLG-ARI
               END-IF
           END-PERFORM
           .
       1110-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動加算１行追加処理
      *****************************************************************
       2001-JIDOTUIKA-SEC     SECTION.
      *
           IF      FLG-KASAN       NOT =   5
               PERFORM 1110-KASAN-CHK-SEC
           END-IF
           IF      FLG-ARI             =   1
               MOVE    IDX-T               TO  WRK-IDX
           ELSE
               EVALUATE    FLG-KASAN
      *            生体乳幼児加算（最後）
                   WHEN    1
                       MOVE    ZERO            TO  FLG-KSN-END
                       PERFORM VARYING     IDK      FROM   IDX-S   BY  1
                               UNTIL      (IDK      >   SPA-MAX-LINE) OR
                                          (FLG-KSN-END  =  1  )
                         IF     ((WRK-COM-INPUTCD (IDK) =   SPACE ) AND
                                 (WRK-GMN-INPUTCD (IDK) =   SPACE ))  OR
                                ( WRK-COM-ZAIEND  (IDK) =   "1"    )
                             MOVE    1               TO  FLG-KSN-END
                             IF   (WRK-COM-INPUTCD (IDK)(1:1)
                                                        =   "1"    )
                                MOVE    IDK             TO  IDX-T
                             END-IF
                         ELSE
                             IF   (WRK-COM-INPUTCD (IDK)(1:1)
                                                        =   "1"    )
                                MOVE    IDK             TO  IDX-T
                             END-IF
                         END-IF
                       END-PERFORM
                       ADD     1               TO  IDX-T
      *            逓減（乳幼児の前）
                   WHEN    2
                       MOVE    ZERO            TO  FLG-KSN-END
                       PERFORM VARYING     IDK      FROM   IDX-S   BY  1
                               UNTIL      (IDK      >   SPA-MAX-LINE) OR
                                          (FLG-KSN-END  =  1  )
      *                  逓減対象の次ぎとする
                         IF      (WRK-COM-INPUTCD (IDK)(1:1)
                                                          =  "1" ) AND
                                 (WRK-COM-TEIGENKBN(IDK)  =  "1" )
                            MOVE    IDK             TO  IDX-T
                         END-IF
                         IF     ((WRK-COM-INPUTCD (IDK) =   SPACE ) AND
                                 (WRK-GMN-INPUTCD (IDK) =   SPACE ))  OR
                                ( WRK-COM-INPUTCD (IDK)(1:1)
                                                    NOT =   "1"    )
                            MOVE    1               TO  FLG-KSN-END
                            IF      IDX-T           =   ZERO
                                MOVE    IDK             TO  IDX-T
                            ELSE
                                ADD     1               TO  IDX-T
                            END-IF
                         END-IF
                       END-PERFORM
      *            コメント（各加算の次）
                   WHEN    4
                       MOVE    ZERO            TO  FLG-KSN-END
                       PERFORM VARYING     IDK     FROM   IDX-S   BY  1
                               UNTIL      (IDK     >   SPA-MAX-LINE) OR
                                          (FLG-KSN-END  =  1  )
                         IF     ((WRK-COM-INPUTCD (IDK) =   SPACE ) AND
                                 (WRK-GMN-INPUTCD (IDK) =   SPACE ))  OR
                                ( WRK-COM-INPUTCD (IDK)(1:1)
                                                    NOT =   "1"    )
                            MOVE    1               TO  FLG-KSN-END
                            MOVE    IDK             TO  IDX-T
                         END-IF
                       END-PERFORM
      *
                   WHEN    OTHER
      *                手技料の次に追加する
                       COMPUTE IDX-T               =   IDX-S   +  1
                       MOVE    ZERO                TO  WRK-IDX
               END-EVALUATE
               MOVE    ZERO                TO  WRK-IDX
           END-IF
      *
           MOVE    ZERO                TO  FLG-ARI
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           IF      WRK-IDX             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-T)
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-T               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
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
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
               MOVE    IDX-S               TO  IDX-T
               GO      TO      2001-JIDOTUIKA-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-ARI
      *
           MOVE    WRK-COM-SRYKBN (IDX-T - 1)
                                       TO  WRK-COM-SRYKBN(IDX-T)
           MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-T)
           MOVE    WRK-COM-SRYSYUKBN (IDX-T - 1)
                                       TO  WRK-COM-SRYSYUKBN(IDX-T)
      *
           MOVE    SPACE               TO  WRK-COM-ZAIEND (IDX-T - 1)
      *
           MOVE    "1"                 TO  WRK-COM-AUTOKBN (IDX-T)
      *
           ADD     1                   TO  WRK-GMN-MAX
      *
           .
       2001-JIDOTUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    全診療行為入院時チェック処理
      *****************************************************************
       210-NYUIN-SYORI-SEC                SECTION.
      *
           MOVE    ZERO                TO  IDX-JIKAN
           MOVE    ZERO                TO  IDX-TEIGEN
           MOVE    ZERO                TO  IDX-TEIGEN2
           INITIALIZE                      TBL-TEIGEN-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE     )   OR
                              (SPA-ERRCD           NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE  AND
                               WRK-COM-INPUTCD (IDX-Z) =   SPACE )
      *
             IF      WRK-COM-HKNCOMBI(IDX-Z) NOT =   "9999"
      *
      *        時間外緊急院内画像診断加算
               IF      WRK-COM-INPUTCD(IDX-Z)  =   "160000210"
                   MOVE    IDX-Z           TO  IDX-JIKAN
               END-IF
      *
      *        病理診断料チェック
               IF      WRK-COM-INPUTCD (IDX-Z) =   "160155110"
                   PERFORM 1006-SINDAN-CHK-SEC
               END-IF
      *
      *        病理学的検査診断料チェック
               IF      WRK-COM-INPUTCD (IDX-Z) =   "160062310"
                   PERFORM 1007-KENSA-CHK-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
               END-IF
      ****     逓減加算算定チェック
      *        IF     (WRK-COM-SRYKBN    (IDX-Z)   =   "60" )  AND
      *               (WRK-COM-TEIGENKBN (IDX-Z)   =   1    )  AND
      *               (WRK-COM-DATAKBN   (IDX-Z)   =  "1"   )
      *            PERFORM 20018-TEIGEN-INSYORI-SEC
      *****    END-IF
      *
      *        重複算定不可チェック
               IF     (WRK-COM-SRYKBN    (IDX-Z)   =   "60"
                                                   OR  "64"  )  AND
                      (WRK-COM-DATAKBN   (IDX-Z)   =   "1"   )  AND
                      (WRK-COM-TEIGEN-FLG(IDX-Z)   =   ZERO )
                   PERFORM 20011-JYUFUKU-CHK-SEC
               END-IF
             END-IF
           END-PERFORM
      *
      *    時間外緊急院内画像診断加算有り
           IF      IDX-JIKAN           >   ZERO
      *        時間入力追加
               MOVE    "840000044"         TO  WRK-SRYCD
               IF      WRK-COM-INPUTCD(IDX-JIKAN + 1)  =   WRK-SRYCD
                                                       OR  CONS-JIKAN
                   CONTINUE
               ELSE
                   MOVE    IDX-JIKAN           TO  IDX-S
                   MOVE    ZERO                TO  FLG-KASAN
                   PERFORM 2001-JIDOTUIKA-SEC
                   IF      FLG-ARI             =   1
                       MOVE    "2"             TO  WRK-COM-AUTOKBN
                                                              (IDX-T)
                       MOVE    "1"             TO  WRK-COM-IN  (IDX-T)
                       MOVE    "2"             TO  WRK-COM-IN2 (IDX-T)
                   END-IF
               END-IF
           END-IF
      *
           .
       210-NYUIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院時２回目以降の逓減処理
      *****************************************************************
       20018-TEIGEN-INSYORI-SEC                SECTION.
      *
      *    同一検査算定有無の判定
           SET     KEN-IDX             TO  1
           SEARCH      TBL-KENSA-HOU-OCC   VARYING   KEN-IDX
                   AT  END
                       MOVE    SPACE           TO  WRK-KEN-HOU
                   WHEN    WRK-COM-INPUTCD (IDX-Z)
                                               =   TBL-KEN-SRYCD
                                                           (KEN-IDX)
                       MOVE    TBL-KEN-HOU  (KEN-IDX)
                                                TO  WRK-KEN-HOU
           END-SEARCH
      *
           MOVE    ZERO                TO  WRK-SANTEI-CNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   KEN-IDX-MAX)  OR
                              (WRK-KEN-HOU     =   SPACE  )  OR
                              (SPA-ERRCD   NOT =   SPACE  )  OR
                              (WRK-SANTEI-CNT  >   ZERO   )
               SET     KEN-IDX             TO  IDX
               IF     (TBL-KEN-HOU  (KEN-IDX)  =   WRK-KEN-HOU ) AND
                      (TBL-KEN-SRYCD(KEN-IDX)
                                           NOT =   WRK-COM-INPUTCD
                                                         (IDX-Z))
                   MOVE    TBL-KEN-SRYCD (KEN-IDX) TO  WRK-SRYCD
                   PERFORM 20081-SANTEI-TBLCHK-SEC
               END-IF
           END-PERFORM
      *
           MOVE    ZERO            TO  IDX-TEI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   20  )  OR
                              (IDX-TEI     >   ZERO)
               IF      WRK-KEN-HOU         =   SPACE
                   IF     (TBL-TEI-SRYCD(IDX)  =   SPACE )  AND
                          (TBL-TEI-HOU  (IDX)  =   SPACE )
                       MOVE    WRK-COM-INPUTCD (IDX-Z)
                                               TO  TBL-TEI-SRYCD(IDX)
                   END-IF
                   IF      TBL-TEI-SRYCD(IDX)  =
                                               WRK-COM-INPUTCD (IDX-Z)
                       MOVE    IDX             TO  IDX-TEI
                   END-IF
               ELSE
                   IF     (TBL-TEI-SRYCD(IDX)  =   SPACE )  AND
                          (TBL-TEI-HOU  (IDX)  =   SPACE )
                       MOVE    WRK-KEN-HOU     TO  TBL-TEI-HOU (IDX)
                   END-IF
                   IF      TBL-TEI-HOU(IDX)    =   WRK-KEN-HOU
                       MOVE    IDX             TO  IDX-TEI
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDX-TEI             >   ZERO
               COMPUTE WRK-SANTEI-CNT      =   WRK-SANTEI-CNT
                                           +   TBL-TEI-CNT (IDX-TEI)
           END-IF
      *    逓減のない時
           IF      WRK-COM-TEIGEN-FLG(IDX-Z)   =   ZERO
      *        逓減加算自動追加
               IF      WRK-SANTEI-CNT      >   ZERO
                   MOVE    IDX-Z               TO  IDX-TEIGEN
                   MOVE    IDX-Z               TO  IDX-TEIGEN2
                   PERFORM 2009-TEIGEN-TUIKA-SEC
                   IF      FLG-ARI             =   1
                       ADD     1               TO  IDX-Z
                   END-IF
               ELSE
                   IF      WRK-COM-NYU-KAI (IDX-Z) >   1
                       MOVE    "9021"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
      *            逓減済み
                   ADD     WRK-COM-NYU-KAI (IDX-Z)
                                               TO  TBL-TEI-CNT (IDX-TEI)
               END-IF
           ELSE
      *        逓減済み
               ADD     WRK-COM-NYU-KAI (IDX-Z) TO  TBL-TEI-CNT (IDX-TEI)
           END-IF
           .
      *
       20018-TEIGEN-INSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自動算定処理
      *****************************************************************
       300-AUTO-SYORI-SEC                SECTION.
      *
      *    包括まとめ入力は以下対象外
          IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      300-AUTO-SYORI-EXT
           END-IF
      *
      *    訂正で自動発生なしの場合
           IF     (LNK-ORCSC-SYORIFLG  =   1  )  AND
                  (SPA-TEISEI-FLG      =   "0")
               GO      TO      300-AUTO-SYORI-EXT
           END-IF
      *
      *    外来迅速検体検査設定情報
           PERFORM 3009-SYS1016-HEN-SEC
      *
           MOVE    ZERO                TO  IDXS
      *
           MOVE    ZERO                TO  FLG-BV
           MOVE    ZERO                TO  FLG-BA
           MOVE    ZERO                TO  FLG-BC
           MOVE    ZERO                TO  FLG-FUKA
      *
           MOVE    ZERO                TO  FLG-BV-ARI
           MOVE    ZERO                TO  FLG-BA-ARI
           MOVE    ZERO                TO  FLG-BC-ARI
      *    検体検査管理加算
           MOVE    ZERO                TO  FLG-KENTAI
           MOVE    ZERO                TO  FLG-KENTAI2
           MOVE    ZERO                TO  FLG-KENTAI-NO
           MOVE    ZERO                TO  WRK-KENTAI2-CNT
           MOVE    ZERO                TO  WRK-KENTAI-DAY
      *    入院時初回加算用
           MOVE    ZERO                TO  WRK-HOU-DAY
           MOVE    ZERO                TO  FLG-HOUKNS-ARI
           MOVE    ZERO                TO  FLG-SYOKAI-ARI
      *
           MOVE    ZERO                TO  WRK-KENIDX
           INITIALIZE                  WRK-KENSA-AREA
           INITIALIZE                  WRK-NYUINDAY-AREA
           INITIALIZE                  TBL-NYUINDAY-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE  AND
                               WRK-COM-INPUTCD (IDX)   =   SPACE )
      *
             IF      WRK-COM-HKNCOMBI(IDX) NOT =   "9999"
      *
      *        検査の時、検査等実施判断を行う
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1"  )  AND
                      (WRK-COM-SRYKBN (IDX)        =   "60"
                                                   OR  "64" )
                   IF      WRK-COM-KNSJISGRPKBN (IDX)    =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    WRK-COM-KNSJISGRPKBN (IDX)
                                           TO  WRK-KENIDX
                       ADD     1           TO  WRK-KENSAGRP(WRK-KENIDX)
      *                保険・科
                       IF      WRK-KEN-SRYKA (WRK-KENIDX)  =   SPACE
                           IF      WRK-COM-SRYKA (IDX)     =   SPACE
                               MOVE    SPA-SRYKA   TO  WRK-KEN-SRYKA
                                                           (WRK-KENIDX)
                           ELSE
                               MOVE    WRK-COM-SRYKA (IDX)
                                                   TO  WRK-KEN-SRYKA
                                                           (WRK-KENIDX)
                           END-IF
                       END-IF
                       IF      WRK-KEN-HKNCOMBI (WRK-KENIDX)   =   SPACE
                           IF      WRK-COM-HKNCOMBI (IDX)  =   SPACE
                               MOVE    SPA-HKNCOMBINUM
                                                   TO  WRK-KEN-HKNCOMBI
                                                           (WRK-KENIDX)
                               MOVE    SPA-CHIKENKBN
                                                   TO  WRK-KEN-CHIKENKBN
                                                           (WRK-KENIDX)
                           ELSE
                               MOVE    WRK-COM-HKNCOMBI (IDX)
                                                   TO  WRK-KEN-HKNCOMBI
                                                           (WRK-KENIDX)
                               MOVE    WRK-COM-CHIKENKBN (IDX)
                                                   TO  WRK-KEN-CHIKENKBN
                                                           (WRK-KENIDX)
                           END-IF
                       END-IF
      *****            検体検査加算
      *                IF      WRK-KENIDX          >=  1  AND  <=  6
      *                    PERFORM 3004-KENTAI-CHK-SEC
      *******          END-IF
      *
      *                判断料の判定
      *---(01.00.02)   LINE UPD START  ---------------------------------
      *****************IF      WRK-COM-KNSJISGRPKBN (IDX)  =   2
                       IF      WRK-COM-KNSJISKBN    (IDX)  =   2
      *---(01.00.02)   LINE UPD END    ---------------------------------
                           ADD     1       TO  WRK-KEN-KBN (WRK-KENIDX)
                       END-IF
                   END-IF
      *            外来迅速検体検査加算
                   PERFORM 3004-KENTAI-CHK-SEC
      *
      *            採血料区分判定
                   EVALUATE    WRK-COM-SAIKETUKBN (IDX)
      *                抹消採血
                       WHEN    1
                           MOVE    1           TO  FLG-BC
                           MOVE    1           TO  FLG-BC-ARI
                           IF      IDX-BC      =   ZERO
                               MOVE    IDX         TO  IDX-BC
                           END-IF
      *                静脈採血
                       WHEN    2
                           MOVE    1           TO  FLG-BV
                           MOVE    1           TO  FLG-BV-ARI
                           IF      IDX-BV      =   ZERO
                               MOVE    IDX         TO  IDX-BV
                           END-IF
      *                動脈採血
                       WHEN    3
                           MOVE    1           TO  FLG-BA
                           MOVE    1           TO  FLG-BA-ARI
                           IF      IDX-BA      =   ZERO
                               MOVE    IDX         TO  IDX-BA
                           END-IF
                   END-EVALUATE
               END-IF
      *
      *        採血料算定不可のチェック
      *        （悪性腫瘍特異物質治療管理料を算定時、自動算定なし）
               IF      WRK-COM-INPUTCD (IDX)      =   "113001210"  OR
                                                      "113001310"  OR
                                                      "113002110"
      *          特定薬剤治療管理料
                                                   OR "113000410"
                                                   OR "113000510"
                                                   OR "113000670"
                                                   OR "113000770"
                                                   OR "180000110"
                                                   OR "180000210"
                   MOVE    1               TO  FLG-FUKA
               END-IF
      *        入院時
               IF      (SPA-NYUGAIKBN          =   "1" )
      *            検体検査１０項目以上算定
                   IF     (WRK-COM-HOUKNS-10 (IDX) =   1  )
                       MOVE    1                   TO  FLG-HOUKNS-ARI
                   END-IF
      *            入院時初回加算算定
                   IF      WRK-COM-INPUTCD (IDX)   =   "160161510"
                       MOVE    1                   TO  FLG-SYOKAI-ARI
                   END-IF
                END-IF
      *        検査で入院回数の時、回数集計
               IF     (SPA-NYUGAIKBN               =   "1" )  AND
                      (WRK-COM-SRYKBN (IDX)(1:1)   =   "6" )  AND
                     ((WRK-GMN-INPUTCD(IDX)(1:1)   =   "*" ) OR
                      (WRK-COM-KAIFLG (IDX)        =   "1" ) OR
                      (WRK-COM-ZAIEND (IDX)        =   "1" ))
                   PERFORM 3009-NYUINKAISU-SEC
               END-IF
               IF      WRK-COM-ZAIEND (IDX)    =   "1"
                   MOVE    ZERO                TO  FLG-BC-ARI
                   MOVE    ZERO                TO  FLG-BV-ARI
                   MOVE    ZERO                TO  FLG-BA-ARI
                   MOVE    ZERO                TO  WRK-KENIDX
                   MOVE    ZERO                TO  FLG-HOUKNS-ARI
               END-IF
             END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   20
               IF     (WRK-KENSAGRP (IDX)  NOT =   ZERO )  AND
                      (WRK-KEN-KBN  (IDX)      =   ZERO )
                   EVALUATE    IDX
      *                尿・糞便判断料
                       WHEN    01
                           MOVE    "160061710"     TO  WRK-KEN-SRYCD
                                                                (IDX)
                       WHEN    02
      *                血液学的判断料
                           MOVE    "160061810" TO  WRK-KEN-SRYCD
                                                                (IDX)
                       WHEN    03
      *                生化学的（１）判断料
                           MOVE    "160061910" TO  WRK-KEN-SRYCD
                                                                (IDX)
                       WHEN    04
      *                生化学的（２）判断料
                           MOVE    "160062010" TO  WRK-KEN-SRYCD
                                                               (IDX)
                       WHEN    05
      *                免疫学的判断料
                           MOVE    "160062110" TO  WRK-KEN-SRYCD
                                                                (IDX)
                       WHEN    06
      *                微生物学的判断料
                           MOVE    "160062210" TO  WRK-KEN-SRYCD
                                                                (IDX)
      *                病理学的検査診断料
                       WHEN    07
                           MOVE    "160062310"     TO  WRK-KEN-SRYCD
                                                                (IDX)
      *                呼吸機能等検査診断料
                       WHEN    11
                           MOVE    "160146910"     TO  WRK-KEN-SRYCD
                                                                (IDX)
      *                脈波図、心機図、ポリグラフ
                       WHEN    12
                           MOVE    "160147010"     TO  WRK-KEN-SRYCD
                                                                (IDX)
      *                脳波検査
                       WHEN    13
                           MOVE    "160147610"     TO  WRK-KEN-SRYCD
                                                                (IDX)
      *                神経・筋検査
                       WHEN    14
                           MOVE    "160147710"     TO  WRK-KEN-SRYCD
                                                                (IDX)
      *                ラジオアイソトープ検査
                       WHEN    15
                           MOVE    "160147910"     TO  WRK-KEN-SRYCD
                                                                (IDX)
                   END-EVALUATE
                END-IF
           END-PERFORM
      *
      *    ＳＰＡを保存する
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
           MOVE    ZERO                TO  IDXS-KENTAI
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   20    )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      WRK-KEN-SRYCD (IDX) NOT =   SPACE
      *
      *            共通チェック処理
                   MOVE    WRK-KEN-SRYCD (IDX) TO  WRK-SRYCD
                   PERFORM 3001-KYOUTU-CHEK-SEC
      *
      *D           MOVE    WRK-KEN-SRYCD (IDX)     TO  TNS-SRYCD
      *D           PERFORM 910-TENSU-READ-SEC
      *            病理学的検査診断料チェック
                   IF    (FLG-ERR              =   ZERO  )  AND
                         (WRK-KEN-SRYCD (IDX)  =   "160062310" )
                       PERFORM 1007-KENSA-CHK-SEC
      *
      *D               MOVE    WRK-KEN-SRYCD (IDX) TO  TNS-SRYCD
      *D               PERFORM 910-TENSU-READ-SEC
      *
                       IF      LNK-SC91-ERRCD      =   SPACE
      *                    追加あり
                           MOVE    ZERO            TO  FLG-ERR
                       ELSE
      *                    追加なし
                           MOVE    1               TO  FLG-ERR
                           MOVE    SPACE           TO  SPA-ERRCD
                       END-IF
                   END-IF
                   IF      FLG-ERR         =   ZERO
                       MOVE    WRK-KEN-SRYCD (IDX) TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
      *
                       ADD     1               TO  IDXS
                       MOVE    WRK-KEN-SRYKA (IDX)
                                               TO  LNK-ORCSC-SRYKA
                                                   (07 IDXS)
                       MOVE    WRK-KEN-HKNCOMBI (IDX)
                                               TO  LNK-ORCSC-HKNCOMBI
                                                   (07 IDXS)
                       MOVE    WRK-KEN-CHIKENKBN(IDX)
                                               TO  LNK-ORCSC-CHIKENKBN
                                                   (07 IDXS)
                       MOVE    WRK-KEN-SRYCD (IDX)
                                               TO  LNK-ORCSC-SRYCD
                                                   (07 IDXS)
                       MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU
                                                   (07 IDXS)
      **********       MOVE    "600"           TO  LNK-ORCSC-SRYSYUKBN
                       MOVE    TNS-SRYSYUKBN   TO  LNK-ORCSC-SRYSYUKBN
                                                   (07 IDXS)
      ***********      MOVE    "60"            TO  LNK-ORCSC-SRYKBN
                       MOVE    TNS-SRYKBN      TO  LNK-ORCSC-SRYKBN
                                                   (07 IDXS)
      *
                       MOVE    "4"             TO  LNK-ORCSC-AUTOKBN
                                                   (07 IDXS)
      *                検体検査管理加算用
                       IF      IDX                 >=  1  AND  <=  6
                           MOVE    1               TO  FLG-KENTAI
                           IF      IDXS-KENTAI     =   ZERO
                               MOVE    IDXS            TO  IDXS-KENTAI
                           END-IF
                       END-IF
      *                入院回数
                       IF      WRK-KEN-DAY (IDX)   NOT =   ZERO
                           MOVE    "*/"        TO  LNK-ORCSC-INPUTCD
                                                    (07 IDXS 01)
                           MOVE    WRK-KEN-DAY (IDX)
                                               TO  LNK-ORCSC-INPUTCD
                                                    (07 IDXS 01)(3:2)
      *                検体検査管理加算用
                           IF      IDX                 >=  1   OR  <=  6
                               IF      (WRK-KENTAI-DAY  =   ZERO  )  OR
                                       (WRK-KEN-DAY (IDX)
                                                   <   WRK-KENTAI-DAY)
                                   MOVE    WRK-KEN-DAY (IDX)
                                                   TO  WRK-KENTAI-DAY
                               END-IF
                           END-IF
                       END-IF
      *                算定履歴
                       IF      WRK-SRYCD (1:1)     =   "1"
                           MOVE    TNS-SANTEIRRKKBN
                                           TO  LNK-ORCSC-SANTEIRRKKBN
                                                          (07 IDXS)
                       END-IF
      *
      *                乳幼児加算セット
                       PERFORM 30051-NENREI-SET-SEC
      *                併用チェック用
                       ADD     1               TO  WRK-GMN-MAX
                       IF      WRK-GMN-MAX     >   SPA-MAX-LINE
                           MOVE    SPA-MAX-LINE    TO  WRK-GMN-MAX
                       END-IF
                       MOVE    WRK-KEN-SRYCD (IDX) TO  WRK-COM-INPUTCD
                                                      (WRK-GMN-MAX)
                   END-IF
               END-IF
           END-PERFORM
      *    検体検査管理加算
           IF      FLG-KENTAI          =   1
               MOVE    LNK-ORCSC-SRYKA (07 IDXS-KENTAI)
                                           TO  WRK-SRYKA
               MOVE    LNK-ORCSC-HKNCOMBI(07 IDXS-KENTAI)
                                           TO  WRK-HKNCOMBI
               MOVE    LNK-ORCSC-CHIKENKBN (07 IDXS-KENTAI)
                                           TO  WRK-CHIKENKBN
      *        検体検査管理加算
               IF      SPA-NYUGAIKBN       =   1
      *            検体検査管理加算３
                   MOVE    "160161610"         TO  WRK-SRYCD
               ELSE
      *            検体検査管理加算１
                   MOVE    "160170170"         TO  WRK-SRYCD
               END-IF
               MOVE    WRK-KENTAI-DAY      TO  WRK-SAI-DAY
               MOVE    SPACE               TO  WRK-AUTOKBN
               PERFORM 3005-AUTO-SAIKETU-SEC
      *        検体管理加算３が算定できない時、２を試す
               IF     (FLG-ERR         NOT =   ZERO )  AND
                      (SPA-NYUGAIKBN       =   1    )
      *            検体検査管理加算２
                   MOVE    "160182770"         TO  WRK-SRYCD
                   MOVE    WRK-KENTAI-DAY      TO  WRK-SAI-DAY
                   MOVE    SPACE               TO  WRK-AUTOKBN
                   PERFORM 3005-AUTO-SAIKETU-SEC
      *            検体管理加算２が算定できない時、１加算する
                   IF     (FLG-ERR         NOT =   ZERO )
                       MOVE    "160170170"         TO  WRK-SRYCD
                       MOVE    WRK-KENTAI-DAY      TO  WRK-SAI-DAY
                       PERFORM 3005-AUTO-SAIKETU-SEC
                   END-IF
               END-IF
           END-IF
      *Ｈ１８／４
      *    外来迅速検体検査加算
           IF     (FLG-KENTAI2         =   1  )  AND
                  (SPA-NYUGAIKBN       =   2  )  AND
                  (SPA-KENSAAUTO-FLG   =   "1"
                                       OR  "2" )
      *    すべてシステム管理に設定されていること
           AND    (FLG-KENTAI-NO       =   ZERO )
               PERFORM 3006-AUTO-KENTAIKASN-SEC
           END-IF
      *VER.4.3
      *    入院時初回加算自動発生（訂正時はなし）
           IF     (SPA-NYUGAIKBN       =   1  )  AND
                  (LNK-ORCSC-SYORIFLG  =   0  )  AND
                  (SPA-NYUKENSA-FLG    =   "1"  )  AND
                  (WRK-HOU-DAY         >   ZERO )  AND
                  (FLG-SYOKAI-ARI      =   ZERO )
               PERFORM 3008-AUTO-NYUSYOKAI-SEC
           END-IF
      *
      *    採血料自動発生なし
           IF      FLG-FUKA            =   1
      *        ＳＰＡを元へ戻す
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
               GO      TO      300-AUTO-SYORI-EXT
           END-IF
      *
      *    採血料自動発生
           IF      FLG-BA              =   1
               MOVE    WRK-COM-SRYKA (IDX-BA)
                                           TO  WRK-SRYKA
               MOVE    WRK-COM-HKNCOMBI(IDX-BA)
                                           TO  WRK-HKNCOMBI
               MOVE    WRK-COM-CHIKENKBN (IDX-BA)
                                           TO  WRK-CHIKENKBN
      *        動脈血採血
               MOVE    "160101210"         TO  WRK-SRYCD
               IF      SPA-NYUGAIKBN       =   "1"
                   PERFORM 3007-BA-DAY-SEC
               ELSE
                   MOVE    WRK-BA-DAY          TO  WRK-SAI-DAY
                   PERFORM 3005-AUTO-SAIKETU-SEC
               END-IF
      *        ＳＰＡを元へ戻す
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
      *!!!!!!!!GO      TO      300-AUTO-SYORI-EXT
           END-IF
           IF      FLG-BV              =   1
               MOVE    WRK-COM-SRYKA (IDX-BV)
                                           TO  WRK-SRYKA
               MOVE    WRK-COM-HKNCOMBI(IDX-BV)
                                           TO  WRK-HKNCOMBI
               MOVE    WRK-COM-CHIKENKBN(IDX-BV)
                                           TO  WRK-CHIKENKBN
      *        静脈血採血
               MOVE    "160095710"         TO  WRK-SRYCD
               MOVE    WRK-BV-DAY          TO  WRK-SAI-DAY
               PERFORM 3005-AUTO-SAIKETU-SEC
      *        ＳＰＡを元へ戻す
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
               GO      TO      300-AUTO-SYORI-EXT
           END-IF
           IF      FLG-BC              =   1
               MOVE    WRK-COM-SRYKA (IDX-BC)
                                           TO  WRK-SRYKA
               MOVE    WRK-COM-HKNCOMBI(IDX-BC)
                                           TO  WRK-HKNCOMBI
               MOVE    WRK-COM-CHIKENKBN(IDX-BC)
                                           TO  WRK-CHIKENKBN
      *        抹消採血
               MOVE    "160095810"         TO  WRK-SRYCD
               MOVE    WRK-BC-DAY          TO  WRK-SAI-DAY
               PERFORM 3005-AUTO-SAIKETU-SEC
      *        ＳＰＡを元へ戻す
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
               GO      TO      300-AUTO-SYORI-EXT
           END-IF
      *
           MOVE    HZN-SCR-REC         TO  WRK-SCR-REC
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    外来迅速検体検査加算チェック処理
      *****************************************************************
       3009-SYS1016-HEN-SEC              SECTION.
      *
      *    外来迅速検体検査設定情報
           MOVE    SPACE               TO  SYS-1016-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1016-HOSPNUM
           MOVE    "1016"              TO  SYS-1016-KANRICD
           MOVE    "1"                 TO  SYS-1016-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1016-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1016-EDYUKYMD
      *
           MOVE    SYS-1016-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1016-REC
           ELSE
               INITIALIZE                  SYS-1016-REC
           END-IF
      *
           .
       3009-SYS1016-HEN-EXT.
           EXIT.
      *****************************************************************
      *    外来迅速検体検査加算チェック処理
      *****************************************************************
       3004-KENTAI-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-KENTAI-CHK
           SET     TAI-IDX             TO  1
           SEARCH      TBL-KENSA-TAI-OCC   VARYING   TAI-IDX
                   AT  END
                       MOVE    ZERO                TO  FLG-KENTAI-CHK
                   WHEN    WRK-COM-INPUTCD (IDX)
                                               =   TBL-TAI-SRYCD
                                                           (TAI-IDX)
                       MOVE    1                   TO  FLG-KENTAI-CHK
           END-SEARCH
           IF      FLG-KENTAI-CHK      =   1
      *        システム管理設定分を対象とする
               IF      SPA-KENSAAUTO-FLG   =   "2"
                   PERFORM 30041-KENSAAUTO-CHK-SEC
               END-IF
           END-IF
           IF      FLG-KENTAI-CHK      =   1
               IF      FLG-KENTAI2         =   ZERO
                   IF      WRK-COM-SRYKA (IDX)     =   SPACE
                       MOVE    SPA-SRYKA       TO  WRK-KENTAI-SRYKA
                   ELSE
                        MOVE    WRK-COM-SRYKA (IDX)
                                               TO  WRK-KENTAI-SRYKA
                   END-IF
                   IF      WRK-COM-HKNCOMBI (IDX)     =   SPACE
                       MOVE    SPA-HKNCOMBINUM TO  WRK-KENTAI-HKNCOMBI
                       MOVE    SPA-CHIKENKBN   TO  WRK-KENTAI-CHIKENKBN
                   ELSE
                        MOVE    WRK-COM-HKNCOMBI (IDX)
                                               TO  WRK-KENTAI-HKNCOMBI
                        MOVE    WRK-COM-CHIKENKBN (IDX)
                                               TO  WRK-KENTAI-CHIKENKBN
                   END-IF
               END-IF
      *
               MOVE    1               TO  FLG-KENTAI2
               ADD     1               TO  WRK-KENTAI2-CNT
           END-IF
      *
           .
       3004-KENTAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来迅速検体検査対象判定処理
      *****************************************************************
       30041-KENSAAUTO-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-KENTAI-CHK2
           PERFORM VARYING     IDX-G   FROM    1   BY  1
                   UNTIL      (IDX-G   >   55  )
                        OR    (SYS-1016-SRYCD (IDX-G)  =   SPACE)
                        OR    (FLG-KENTAI-CHK2 =   1    )
               IF      SYS-1016-SRYCD (IDX-G)  =   WRK-COM-INPUTCD (IDX)
                   MOVE    1               TO  FLG-KENTAI-CHK2
               END-IF
           END-PERFORM
      *    システム管理に設定されていない検査の時、自動発生なしとする
           IF      FLG-KENTAI-CHK2      =   ZERO
               MOVE    1                   TO  FLG-KENTAI-NO
               MOVE    ZERO                TO  FLG-KENTAI-CHK
           END-IF
           .
       30041-KENSAAUTO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院回数集計処理
      *****************************************************************
       3009-NYUINKAISU-SEC              SECTION.
      *
           IF       WRK-GMN-INPUTCD(IDX)(1:1)  NOT =   "*"
               MOVE    SPA-SRYYMD (7:2)    TO  IDN1
           ELSE
               MOVE    ZERO                TO  IDN1
               PERFORM VARYING     IDN2    FROM    1   BY  1
                       UNTIL       IDN2    >   31
                   IF      WRK-COM-NYUINDAY (IDX IDN2) >   ZERO
                       MOVE    IDN2            TO  IDN1
                       MOVE    31              TO  IDN2
                   END-IF
               END-PERFORM
               IF      IDN1            =   ZERO
                   MOVE    SPA-SRYYMD (7:2)    TO  IDN1
               END-IF
           END-IF
      *    判断料の時、初日を設定
           IF      WRK-KENIDX          >   ZERO
      *        入院時の初日
               IF     (WRK-KEN-DAY (WRK-KENIDX)    =   ZERO )  OR
                      (WRK-KEN-DAY (WRK-KENIDX)    >  IDN1  )
                   MOVE    IDN1            TO  WRK-KEN-DAY  (WRK-KENIDX)
               END-IF
           END-IF
      *    採血料
           IF      FLG-BV-ARI          =   1
      *        入院時の初日
               IF     (WRK-BV-DAY      =   ZERO )  OR
                      (WRK-BV-DAY      >   IDN1 )
                   MOVE    IDN1            TO  WRK-BV-DAY
               END-IF
           END-IF
      *    採血料
           IF      FLG-BC-ARI          =   1
      *        入院時の初日
               IF     (WRK-BC-DAY      =   ZERO )  OR
                      (WRK-BC-DAY      >   IDN1 )
                   MOVE    IDN1            TO  WRK-BC-DAY
               END-IF
           END-IF
      *    入院時初回加算
           IF      FLG-HOUKNS-ARI      =   1
      *        入院時の初日
               IF     (WRK-HOU-DAY     =   ZERO )  OR
                      (WRK-HOU-DAY     >   IDN1 )
                   MOVE    IDN1            TO  WRK-HOU-DAY
               END-IF
           END-IF
      *    採血料（動脈）
           IF      FLG-BA-ARI          =   1
               MOVE    1               TO  TBL-NYUINDAY(IDN1)
               IF      WRK-GMN-INPUTCD(IDX)(1:1)  =   "*"
                   PERFORM VARYING     IDN2    FROM    1   BY  1
                           UNTIL       IDN2    >   31
                       IF      WRK-COM-NYUINDAY (IDX IDN2) >   ZERO
                           MOVE    IDN2        TO  IDN1
                           MOVE    1           TO  TBL-NYUINDAY(IDN1)
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       3009-NYUINKAISU-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
      *3001-SANTEI-CHK-SEC         SECTION.
      *
      *----(01.00.01) LINE UPD START ----------------------------------
      *    今回入力にないこと
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
      *                       (FLG-ERR              =   1    )   OR
      *                       (WRK-GMN-INPUTCD(IDX) =   SPACE)
      *        IF      WRK-GMN-INPUTCD(IDX)    =   WRK-KEN-SRYCD (IDX)
      *            MOVE    1                   TO  FLG-ERR
      *        END-IF
      *****END-PERFORM
      *    MOVE    ZERO                TO  FLG-ERR
      *
      *    PERFORM VARYING     IDX-CHK FROM    1   BY  1
      *            UNTIL      (IDX-CHK >   SPA-MAX-LINE  )  OR
      *                       (FLG-ERR              =   1    )   OR
      *                       (WRK-GMN-INPUTCD(IDX-CHK) =   SPACE)
      *        IF      WRK-GMN-INPUTCD(IDX-CHK)    =   WRK-SRYCD
      *            MOVE    1                   TO  FLG-ERR
      *        END-IF
      *    END-PERFORM
      *----(01.00.01) LINE UPD END   ----------------------------------
      *    IF      FLG-ERR             =   1
      *        GO      TO      3001-SANTEI-CHK-EXT
      *    END-IF
      *
      *
      *    INITIALIZE                      ORCSC91AREA
      *    MOVE    "2"                 TO  LNK-SC91-KBN
      *    MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
      *    MOVE    WRK-SRYCD           TO  LNK-SC91-SRYCD
      *    CALL    "ORCSC91"       USING
      *                                MCPAREA
      *                                ORCSC91AREA
      *                                WRK-SCR-REC
      *                                SPA-AREA
      *                                TNS-TENSU-REC
      *
      *    IF      LNK-SC91-ERRCD      =   SPACE
      *        追加あり
      *        MOVE    ZERO            TO  FLG-ERR
      *    ELSE
      *        追加なし
      *        MOVE    1               TO  FLG-ERR
      *    END-IF
      *
      *    .
      *3001-SANTEI-CHK-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    共通チェック処理
      *****************************************************************
       3001-KYOUTU-CHEK-SEC         SECTION.
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "3"                 TO  LNK-SC92-KBN
           COMPUTE LNK-SC92-GMN-IDX    =   WRK-GMN-MAX
                                       +   1
           IF      LNK-SC92-GMN-IDX    >   SPA-MAX-LINE
               MOVE    SPA-MAX-LINE        TO  LNK-SC92-GMN-IDX
           END-IF
      *
           IF      WRK-COM-INPUTCD (LNK-SC92-GMN-IDX)
                                       NOT =   SPACE
               INITIALIZE              WRK-GMN-TBL (LNK-SC92-GMN-IDX)
           END-IF
      *
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
           MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           MOVE    LNK-ORCSC-KT01-AREA     TO  LNK-SC92-KT01-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    エラーの時、追加なし
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
      *        追加なし
               MOVE    1               TO  FLG-ERR
           ELSE
      *        追加あり
               MOVE    ZERO            TO  FLG-ERR
           END-IF
      *
           .
       3001-KYOUTU-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    採血料自動発生処理
      *****************************************************************
       3005-AUTO-SAIKETU-SEC         SECTION.
      *
      *    同一コードチェック
           PERFORM 30051-INPUT-CHECK-SEC
      *    共通チェック処理
           IF      FLG-ERR             =   ZERO
               PERFORM 3001-KYOUTU-CHEK-SEC
           END-IF
      *
           IF      FLG-ERR         =   ZERO
      *
               MOVE    WRK-SRYCD           TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
      *
               ADD     1               TO  IDXS
               MOVE    WRK-SRYKA       TO  LNK-ORCSC-SRYKA
                                                   (07 IDXS)
               MOVE    WRK-HKNCOMBI    TO  LNK-ORCSC-HKNCOMBI
                                                   (07 IDXS)
               MOVE    WRK-CHIKENKBN   TO  LNK-ORCSC-CHIKENKBN
                                                   (07 IDXS)
               MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD
                                                   (07 IDXS)
               MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU
                                                   (07 IDXS)
               MOVE    "600"           TO  LNK-ORCSC-SRYSYUKBN
                                                   (07 IDXS)
               MOVE    "60"            TO  LNK-ORCSC-SRYKBN
                                                   (07 IDXS)
      *
               MOVE    "4"             TO  LNK-ORCSC-AUTOKBN
                                                   (07 IDXS)
               IF      WRK-AUTOKBN     NOT =   SPACE
                   MOVE    WRK-AUTOKBN     TO  LNK-ORCSC-AUTOKBN
                                                   (07 IDXS)
               END-IF
      *        入院回数
               IF      WRK-SAI-DAY     NOT =   ZERO
                    MOVE    "*/"           TO  LNK-ORCSC-INPUTCD
                                                    (07 IDXS 01)
                    MOVE    WRK-SAI-DAY    TO  LNK-ORCSC-INPUTCD
                                                    (07 IDXS 01)(3:2)
               END-IF
      *        算定履歴
               IF      WRK-SRYCD (1:1)     =   "1"
                   MOVE    TNS-SANTEIRRKKBN
                                           TO  LNK-ORCSC-SANTEIRRKKBN
                                                          (07 IDXS)
               END-IF
      *        併用チェック用
               ADD     1               TO  WRK-GMN-MAX
               IF      WRK-GMN-MAX     >   SPA-MAX-LINE
                    MOVE    SPA-MAX-LINE   TO  WRK-GMN-MAX
               END-IF
               MOVE    WRK-SRYCD       TO  WRK-COM-INPUTCD
                                              (WRK-GMN-MAX)
      *        乳幼児加算セット
               PERFORM 30051-NENREI-SET-SEC
           END-IF
      *
           .
       3005-AUTO-SAIKETU-EXT.
           EXIT.
      *****************************************************************
      *    同一コードチェック処理
      *****************************************************************
       30051-INPUT-CHECK-SEC         SECTION.
      *
      *    入力判定
           MOVE    ZERO                TO  FLG-ERR
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE )   OR
                              (FLG-ERR     NOT         =   ZERO)  OR
                             ((WRK-GMN-INPUTCD (IDX-Z) =   SPACE ) AND
                              (WRK-COM-INPUTCD (IDX-Z) =   SPACE ))
      *
               IF     (WRK-COM-HKNCOMBI(IDX-Z) NOT =   "9999" )
                 AND  (WRK-COM-SRYKBN  (IDX-Z)     =   "60"   ) 
                 AND  (WRK-COM-INPUTCD (IDX-Z)     =   WRK-SRYCD )
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-PERFORM
           .
       30051-INPUT-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢加算追加処理（共通）
      *****************************************************************
       30051-NENREI-SET-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (TNS-AGEKSNSRYCD(IDX2)   =   SPACE  OR
                                                           ZERO )
               MOVE    ZERO                    TO  FLG-ERR
      *        加算追加
               MOVE    TNS-AGEKSNSRYCD (IDX2)  TO  WRK-SRYCD
      *        上限年齢判定
               IF      TNS-AGEKSNJGN (IDX2)     =   "AA"
      *            新生児判定
                   IF     (LNK-ORCSC-NENREI-YY    =   ZERO )  AND
                          (LNK-ORCSC-NENREI-MM    =   ZERO )  AND
                          (LNK-ORCSC-NENREI-DD    <   28   )
                       CONTINUE
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
               ELSE
                   MOVE    TNS-AGEKSNJGN (IDX2)    TO  WRK-TNS-JGNAGE
                   IF     (WRK-TNS-JGNAGE      NOT =   ZERO   )  AND
                          (LNK-ORCSC-NENREI-YY     <   WRK-TNS-JGNAGE )
                       CONTINUE
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
               END-IF
               IF      FLG-ERR             =   ZERO
      *        共通チェック処理
               PERFORM 3001-KYOUTU-CHEK-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-SRYCD           TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF      FLG-TENSU           =   ZERO
      *                算定あり
                       MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD-KS
                                                              (07 IDXS)
                       MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU-KS
                                                              (07 IDXS)
                       MOVE    4               TO  IDX2
                   END-IF
               END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       30051-NENREI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢チェック処理
      *****************************************************************
       300511-NENREI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-KGNAGE
           MOVE    ZERO                TO  WRK-JGNAGE
           MOVE    TNS-AGEKSNKGN(IDX2) TO  WRK-KGNAGE(2:2)
           MOVE    TNS-AGEKSNJGN(IDX2) TO  WRK-JGNAGE(2:2)
      *
      *    下限年齢チェック
           IF      TNS-AGEKSNKGN(IDX2)     =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      LNK-ORCSC-NENREI-YY     >=  WRK-KGNAGE
                   CONTINUE
               ELSE
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-IF
      *
      *    上限年齢チェック
           IF      TNS-AGEKSNJGN(IDX2) =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               IF      TNS-AGEKSNJGN(IDX2)     =   "AA"
      *            新生児判定
                   IF     (LNK-ORCSC-NENREI-YY    =   ZERO )  AND
                          (LNK-ORCSC-NENREI-MM    =   ZERO )  AND
                          (LNK-ORCSC-NENREI-DD    <   28   )
                       CONTINUE
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
               ELSE
                   IF      LNK-ORCSC-NENREI-YY     <   WRK-JGNAGE
                       CONTINUE
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
               END-IF
           END-IF
      *
           .
       300511-NENREI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    外来検体検査加算自動発生処理
      *****************************************************************
       3006-AUTO-KENTAIKASN-SEC         SECTION.
      *
           MOVE    "160177770"         TO  WRK-SRYCD
      *    同一コードチェック
           PERFORM 30051-INPUT-CHECK-SEC
      *    共通チェック処理
           IF      FLG-ERR             =   ZERO
               PERFORM 3001-KYOUTU-CHEK-SEC
           END-IF
      *
           IF      FLG-ERR         =   ZERO
      *
      *    当日回数
           INITIALIZE                      ORCSC91AREA
           MOVE    "2"                 TO  LNK-SC91-KBN
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
           MOVE    WRK-SRYCD           TO  LNK-SC91-SRYCD
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC91-TEISEI-AREA
           MOVE    LNK-ORCSC-KT01-AREA
                                       TO  LNK-SC91-KT01-AREA
           CALL    "ORCSC91"           USING
                                       MCPAREA
                                       ORCSC91AREA
                                       WRK-SCR-REC
                                       SPA-AREA
                                       TNS-TENSU-REC
      *
           IF      LNK-SC91-DAYCNT     >=  5
               MOVE    1               TO  FLG-INPUT
           ELSE
               IF     (LNK-SC91-DAYCNT +   WRK-KENTAI2-CNT)
                                           >   5
                   COMPUTE WRK-KENTAI2-CNT =   5
                                           -   LNK-SC91-DAYCNT
               END-IF
           END-IF
      *    入力にないとき、検査数で自動発生
      **** IF      FLG-INPUT           =   ZERO
      *        点数マスタ編集・基本チェック
               INITIALIZE                  ORCSC92AREA
               MOVE    "2"                 TO  LNK-SC92-KBN
               COMPUTE WRK-GMN-MAX     =   WRK-GMN-MAX
                                           +   1
               IF      WRK-GMN-MAX     >   SPA-MAX-LINE
                   MOVE    SPA-MAX-LINE    TO  WRK-GMN-MAX
               END-IF
               MOVE    WRK-GMN-MAX         TO  LNK-SC92-GMN-IDX
               MOVE    WRK-SRYCD           TO  WRK-GMN-INPUTCD
                                                       (WRK-GMN-MAX)
               IF      WRK-KENTAI2-CNT     >   5
                   MOVE    5               TO  WRK-COM-SURYO
                                                     (WRK-GMN-MAX)
               ELSE
                   MOVE    WRK-KENTAI2-CNT TO  WRK-COM-SURYO
                                                    (WRK-GMN-MAX)
               END-IF
               MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
               MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
               MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
               MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
               MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
               MOVE    SPACE               TO  LNK-SC92-LSTSRYKA
               MOVE    SPACE               TO  LNK-SC92-LSTSRYKAMEI
               MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
               MOVE    LNK-ORCSC-KT01-AREA
                                           TO  LNK-SC92-KT01-AREA
               CALL    "ORCSC92"           USING
                                           MCPAREA
                                           ORCSC92AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *    エラーの時、追加なし
               IF     (SPA-ERRCD           NOT =   SPACE )  OR
                      (LNK-SC92-ERRAREA    NOT =   SPACE )
      *            追加なし
                   MOVE    1               TO  FLG-ERR
               ELSE
      *            追加あり
                   MOVE    ZERO            TO  FLG-ERR
                   ADD     1               TO  IDXS
                   MOVE    WRK-KENTAI-SRYKA    TO  LNK-ORCSC-SRYKA
                                                   (07 IDXS)
                   MOVE    WRK-KENTAI-HKNCOMBI TO  LNK-ORCSC-HKNCOMBI
                                                   (07 IDXS)
                   MOVE    WRK-KENTAI-CHIKENKBN
                                           TO  LNK-ORCSC-CHIKENKBN
                                                   (07 IDXS)
                   MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD
                                                   (07 IDXS)
                   MOVE    WRK-COM-KISOTEN (WRK-GMN-MAX)
                                           TO  LNK-ORCSC-TENSU
                                                   (07 IDXS)
                   MOVE    WRK-COM-SURYO  (WRK-GMN-MAX)
                                           TO  LNK-ORCSC-SURYO
                                                   (07 IDXS)
                   MOVE    "600"           TO  LNK-ORCSC-SRYSYUKBN
                                                   (07 IDXS)
                   MOVE    "60"            TO  LNK-ORCSC-SRYKBN
                                                   (07 IDXS)
      *
                   MOVE    "4"             TO  LNK-ORCSC-AUTOKBN
                                                   (07 IDXS)
      *            算定履歴
                   IF      WRK-SRYCD (1:1)     =   "1"
                       MOVE    TNS-SANTEIRRKKBN
                                           TO  LNK-ORCSC-SANTEIRRKKBN
                                                          (07 IDXS)
                   END-IF
               END-IF
               MOVE    HZN-SCR-REC         TO  WRK-SCR-REC 
           END-IF
           .
       3006-AUTO-KENTAIKASN-EXT.
           EXIT.
      *****************************************************************
      *    入院動脈血採血料処理
      *****************************************************************
       3007-BA-DAY-SEC         SECTION.
      *
      *    同一コードチェック
           PERFORM 30051-INPUT-CHECK-SEC
      *    共通チェック処理
           IF      FLG-ERR         =   ZERO
               PERFORM 3001-KYOUTU-CHEK-SEC
           END-IF
      *
           IF      FLG-ERR         =   ZERO
      *
               MOVE    WRK-SRYCD           TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
      *
               ADD     1               TO  IDXS
               MOVE    WRK-SRYKA       TO  LNK-ORCSC-SRYKA
                                                   (07 IDXS)
               MOVE    WRK-HKNCOMBI    TO  LNK-ORCSC-HKNCOMBI
                                                   (07 IDXS)
               MOVE    WRK-CHIKENKBN   TO  LNK-ORCSC-CHIKENKBN
                                                   (07 IDXS)
               MOVE    WRK-SRYCD       TO  LNK-ORCSC-SRYCD
                                                   (07 IDXS)
               MOVE    TNS-TEN         TO  LNK-ORCSC-TENSU
                                                   (07 IDXS)
               MOVE    "600"           TO  LNK-ORCSC-SRYSYUKBN
                                                   (07 IDXS)
               MOVE    "60"            TO  LNK-ORCSC-SRYKBN
                                                   (07 IDXS)
      *
               MOVE    "4"             TO  LNK-ORCSC-AUTOKBN
                                                   (07 IDXS)
      *        算定履歴
               IF      WRK-SRYCD (1:1)     =   "1"
                   MOVE    TNS-SANTEIRRKKBN
                                           TO  LNK-ORCSC-SANTEIRRKKBN
                                                          (07 IDXS)
               END-IF
      *        入院回数
               IF      TBL-NYUINDAY-AREA   NOT =   ZERO
                    PERFORM 30071-BA-DAY-HENSYU-SEC
               END-IF
      *        併用チェック用
               ADD     1               TO  WRK-GMN-MAX
               IF      WRK-GMN-MAX     >   SPA-MAX-LINE
                    MOVE    SPA-MAX-LINE   TO  WRK-GMN-MAX
               END-IF
               MOVE    WRK-SRYCD       TO  WRK-COM-INPUTCD
                                              (WRK-GMN-MAX)
           END-IF
           .
       3007-BA-DAY-EXT.
           EXIT.
      *****************************************************************
      *    入院動脈血採血料処理
      *****************************************************************
       30071-BA-DAY-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDN1
           MOVE    ZERO                TO  IDN2
      *    診療年月の末日算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-SRYYMD(1:6)     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI(7:2)    TO  WRK-MATU
      *
           INITIALIZE                      WRK-NISUU-AREA
           MOVE    ZERO                TO  WRK-H-DAY
           MOVE    SPACE               TO  WRK-H-SEL
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL       IDN1    >   32
               IF     (IDN1                =   32  )  OR
                      (IDN1                >   WRK-MATU)  OR
                      (TBL-NYUINDAY(IDN1)  =   ZERO)
                   IF      WRK-H-DAY       NOT =   ZERO
                       IF      WRK-H-SEL   NOT =   SPACE
                           ADD     1               TO  IDN2
                           MOVE    WRK-H-DAY       TO  WRK-DAY(IDN2)
                           MOVE    WRK-H-SEL       TO  WRK-SEL(IDN2 - 1)
                       END-IF
                       MOVE    ZERO            TO  WRK-H-DAY
                       MOVE    SPACE           TO  WRK-H-SEL
                   END-IF
               ELSE
                   IF      WRK-H-DAY           =   ZERO
                       MOVE    IDN1            TO  WRK-H-DAY
                       ADD     1               TO  IDN2
                       MOVE    WRK-H-DAY       TO  WRK-DAY(IDN2)
                   ELSE
                       IF      WRK-H-DAY       =  (IDN1  -  1 )
                           MOVE    "-"             TO  WRK-H-SEL
                       END-IF
                       MOVE    IDN1            TO  WRK-H-DAY
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL       IDN1    >   31
               IF     (WRK-SEL (IDN1)      =   SPACE )  AND
                      (IDN1                <   31    )
                   IF      WRK-DAY (IDN1 + 1 )     NOT =   ZERO
                       MOVE    ","          TO  WRK-SEL (IDN1)
                   END-IF
               ELSE
                   IF     (IDN1                    =   31    )  OR
                          (WRK-DAY (IDN1 + 1 )     =   ZERO  )
                       MOVE    SPACE        TO  WRK-SEL (IDN1)
                   END-IF
               END-IF
           END-PERFORM
           INITIALIZE                      LNK-ORCSC-INPUTCD-G (07 IDXS)
      *    １日１回とする
           MOVE    SPACE               TO  WRK-KAISU-H
           MOVE    ZERO                TO  IDN4
           MOVE    1                   TO  IDN2
           MOVE    1                   TO  IDN3
           PERFORM VARYING     IDN1    FROM    1   BY  1
                   UNTIL      (IDN1    >   31  )  OR
                              (IDN2            >=  5  )  OR
                              (WRK-DAY (IDN1)  =   ZERO )
               IF      IDN3                >   19
                   ADD     1               TO  IDN2
                   MOVE    1               TO  IDN3
               END-IF
               IF      LNK-ORCSC-INPUTCD(07 IDXS IDN2)   =   SPACE
                   STRING  "*"             DELIMITED  BY  SIZE
                           WRK-KAISU-H     DELIMITED  BY  SPACE
                           "/"             DELIMITED  BY  SIZE
                           INTO            LNK-ORCSC-INPUTCD
                                                   (07 IDXS IDN2)
                   END-STRING
                   ADD     2               TO  IDN3
                   ADD     IDN4            TO  IDN3
               END-IF
               IF      WRK-SEL (IDN1)      =   SPACE
                   MOVE    ","             TO  WRK-SEL (IDN1)
               END-IF
               MOVE    WRK-NISUU-TBL(IDN1) TO  LNK-ORCSC-INPUTCD
                                                  (07 IDXS IDN2)(IDN3:3)
               ADD     3                   TO  IDN3
           END-PERFORM
      *
           .
       30071-BA-DAY-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    入院時初回加算自動発生処理
      *****************************************************************
       3008-AUTO-NYUSYOKAI-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-KENSA-ARI
      *
           MOVE    SPA-SRYYMD          TO  WRK-HOU-YMD
           MOVE    WRK-HOU-DAY         TO  WRK-HOU-YMD(7:2)
      *    今回入力内容チェック
           PERFORM 30080-INPUT-CHK-SEC
      *    算定履歴からの判定
           IF      FLG-KENSA-ARI       =   ZERO
               PERFORM 30081-SANTEIRRK-CHK-SEC
           END-IF
      *    診療行為から検体検査の算定確認
           IF      FLG-KENSA-ARI       =   ZERO
               PERFORM 30082-SRYACT-KENCHK-SEC
           END-IF
      *    入院時初回加算算定
           IF      FLG-KENSA-ARI       =   ZERO
               MOVE    "160161510"         TO  WRK-SRYCD
               MOVE    WRK-HOU-DAY         TO  WRK-SAI-DAY
               MOVE    "7"                 TO  WRK-AUTOKBN
               PERFORM 3005-AUTO-SAIKETU-SEC
           END-IF
           MOVE    SPACE               TO  WRK-AUTOKBN
      *
           .
       3008-AUTO-NYUSYOKAI-EXT.
           EXIT.
      *****************************************************************
      *    今回入力分からの判定処理
      *****************************************************************
       30080-INPUT-CHK-SEC         SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   SPA-MAX-LINE  )
                         OR  ((WRK-COM-INPUTCD (IDX2)  =   SPACE ) AND
                              (WRK-GMN-INPUTCD (IDX2)  =   SPACE ))
                         OR   (FLG-KENSA-ARI       =   1    )
               IF     (WRK-COM-HKNCOMBI (IDX2)     =   "9999" )
                 OR   (WRK-COM-SRYKBN  (IDX2)  NOT =   "60"   )
                 OR   (WRK-COM-DATAKBN (IDX2)  NOT =   "1"    )
                   CONTINUE
               ELSE
                   IF     (WRK-COM-CDKBN-KBN(IDX2)
                                                =   "D"   )  AND
                          (WRK-COM-CDKBN-KBNNUM (IDX2)
                                                =  "007" )
      *                算定日判定
                       MOVE    ZERO                TO  IDN1
                       PERFORM VARYING     IDN2    FROM    1   BY  1
                               UNTIL       IDN2    >   31
                           IF      WRK-COM-NYUINDAY (IDX2 IDN2) >  ZERO
                               MOVE    IDN2            TO  IDN1
                               MOVE    31              TO  IDN2
                           END-IF
                       END-PERFORM
                       IF      IDN1            =   ZERO
                           MOVE    SPA-SRYYMD (7:2)    TO  IDN1
                       END-IF
      *                生体検査１０項目算定日より前日に算定
                       IF     IDN1             <   WRK-HOU-DAY
                           MOVE    1           TO  FLG-KENSA-ARI
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       30080-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴からの判定処理
      *****************************************************************
       30081-SANTEIRRK-CHK-SEC         SECTION.
      *
      *    算定履歴から判断料等の算定確認
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-K02N-NYUINYMD (1:6) TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           PERFORM UNTIL   (FLG-SANTEI     =   1   )  OR
                           (FLG-KENSA-ARI  =   1   )
               MOVE    SANTEI-SRYYM        TO  WRK-YM
               MOVE    ZERO                TO  WRK-DD
               PERFORM VARYING    IDX2     FROM    1   BY  1
                       UNTIL     (IDX2     >   31  )  OR
                                 (FLG-KENSA-ARI    =   1   )
                   IF      SANTEI-DAY (IDX2)   NOT =   ZERO
                       MOVE    IDX2            TO  WRK-DD
                       IF     (WRK-YMD         >=  SPA-K02N-NYUINYMD)
                          AND (WRK-YMD         <=  SPA-K02N-TAIINYMD)
                           PERFORM 30081-SYOKAI-CHK-SEC
                       END-IF
                       IF      WRK-YMD         >   SPA-K02N-TAIINYMD
                           MOVE    31          TO  IDX2
                       END-IF
                   END-IF
               END-PERFORM
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key14"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       30081-SANTEIRRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院時初回算定有無判定
      *****************************************************************
       30081-SYOKAI-CHK-SEC         SECTION.
      *
           EVALUATE    SANTEI-SRYCD
      *        入院時初回加算
               WHEN    "160161510"
      *        基本的検体検査実施料
               WHEN    "160145410"
               WHEN    "160165310"
                   MOVE    1               TO  FLG-KENSA-ARI
      *        生化学的（１）判断料
               WHEN    "160061910"
      *            生体検査１０項目算定日より前日に算定
                   IF     WRK-YMD          <   WRK-HOU-YMD
                       MOVE    1           TO  FLG-KENSA-ARI
                   END-IF
           END-EVALUATE
           .
       30081-SYOKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院時初回算定有無判定
      *****************************************************************
       30082-SRYACT-KENCHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-K02N-NYUINYMD (1:6)
                                       TO  ACCT-STSRYYM
           MOVE    SPA-K02N-TAIINYMD (1:6)
                                       TO  ACCT-EDSRYYM
           MOVE    "60"                TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key50"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key50"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM UNTIL       (FLG-SRYACCT      =   1   )
                           OR  (FLG-KENSA-ARI    =   1   )
               MOVE    ACCT-SRYYM          TO  WRK-YM
               MOVE    ZERO                TO  WRK-DD
               PERFORM VARYING    IDX2     FROM    1   BY  1
                       UNTIL     (IDX2     >   31  )  OR
                                 (FLG-KENSA-ARI    =   1   )
                   IF      ACCT-DAY (1 IDX2)   NOT =   ZERO
                       MOVE    IDX2            TO  WRK-DD
      *                入院期間中
                       IF     (WRK-YMD         >=  SPA-K02N-NYUINYMD)
                          AND (WRK-YMD         <=  SPA-K02N-TAIINYMD)
      *                   算定日より前
                          AND (WRK-YMD         <   WRK-HOU-YMD      )
                           PERFORM 300821-KENSACD-CHK-SEC
                       END-IF
                       IF      WRK-YMD         >   SPA-K02N-TAIINYMD
                           MOVE    31          TO  IDX2
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key50"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key50"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       30082-SRYACT-KENCHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為判定処理
      *****************************************************************
       300821-KENSACD-CHK-SEC        SECTION.
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
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL     (FLG-SRYACT   =   1   )
                        OR   (FLG-KENSA-ARI    =   1  )
               PERFORM VARYING     IDX3    FROM    1   BY  1
                       UNTIL      (IDX3        >   5   )  OR
                                  (SRY-SRYCD(IDX3) =   SPACE )
                             OR   (FLG-KENSA-ARI   =   1  )
                   IF      SRY-SRYCD (IDX3)(1:1)   =   "1"
      *                点数マスタより、区分番号編集
                       MOVE    SRY-SRYCD (IDX3)    TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
      *                血液学的検査
                       IF     (TNS-CDKBN-KBN       =   "D"   ) AND
                              (TNS-CDKBN-KBNNUM    =   "007" )
                           MOVE    1               TO  FLG-KENSA-ARI
                       END-IF
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
       300821-KENSACD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
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
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-NEXT-SEC         SECTION.
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
       900-SRYACT-NEXT-EXT.
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
       940-SINACCT-READ-EXT.
           EXIT.
      *
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
      *
