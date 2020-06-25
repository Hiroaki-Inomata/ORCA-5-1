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
      *    公害レセプト（入院）・基本情報     COPY (CPRCF200.INC) 
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.01    NACL-藤原    09/03/10  特別な関係にある他院歴を初回
      *                                     入院日として起算しレセプト記
      *                                     載を行なう対応
      *  04.05.01    NACL-門脇    10/02/16  予備項目追加
      *  04.05.02    NACL-藤原    11/11/22  同日再入院対応
      *  04.05.03    NACL-門脇    12/04/09  入院略称の文字数拡張
      *  04.07.01    NACL-門脇    12/10/12  合計日数（食事）追加
      *  04.07.02    NACL-門脇    14/05/13  超重症児（者）入院診療加算等の
      *                                     算定フラグ追加
      *  04.08.01    NACL-門脇    16/04/15  平成２８年４月改正対応
      *  04.08.02    NACL-門脇    18/03/27  平成３０年４月改正対応
      *  04.08.03    NACL-門脇    18/04/06  平成３０年４月改正対応
      *
      *  05.00.01    NACL-門脇    19/07/01  リハビリ発症日レセプト記載対応
      *                                     （機能追加）
      ********************************************************************
      *01  RECE200-REC.
           03  RECE200-KEY.
      *医療機関番号
               05  RECE200-HOSPNUM         PIC 9(2).
      *診療年月
               05  RECE200-SRYYM           PIC 9(06).
      *入外区分
               05  RECE200-NYUGAIKBN       PIC X(01).
      *患者ＩＤ
               05  RECE200-PTID            PIC 9(10).
      *レセプト診療科
               05  RECE200-RECEKA          PIC X(02).
      *公費ＩＤ
               05  RECE200-HKNID           PIC 9(10).
      *適用開始年月日
               05  RECE200-TEKSTYMD        PIC X(08). 
      *
      *レセプト種別
           03  RECE200-RECESYUBETU         PIC 9(04).
      *
      *レセプト対象年月日情報
           03  RECE200-TEKYMD.
      *適用開始年月日
               05  RECE200-TEKSTYMD-X      PIC X(08). 
      *適用終了年月日
               05  RECE200-TEKEDYMD-X      PIC X(08).
      *
      *患者情報
           03  RECE200-PTINF.
      *患者番号  
               05  RECE200-PTNUM           PIC X(20).
      *カナ氏名
               05  RECE200-KANANAME        PIC X(50).
      *漢字氏名
               05  RECE200-NAME            PIC X(100).
      *性別
               05  RECE200-SEX             PIC X(01).
      *生年月日
               05  RECE200-BIRTHDAY        PIC 9(08).
      *死亡区分
               05  RECE200-DEATHKBN        PIC X(01).
      *年齢
               05  RECE200-AGE             PIC 9(03).
      *テスト患者番号区分
               05  RECE200-TSTPTNUMKBN     PIC X(01).
      *
      *主保険情報
           03  RECE200-HKNINF.
      *主保険−保険番号
               05  RECE200-HKNNUM          PIC X(03).
      *主保険−保険者番号
               05  RECE200-HKNJANUM        PIC X(08).
      *主保険−記号
               05  RECE200-KIGO            PIC X(80).
      *主保険−番号
               05  RECE200-NUM             PIC X(80).
      *
      *診療科情報
           03  RECE200-SRYKAINF            OCCURS  20.
      *診療科
               05  RECE200-SRYKA           PIC X(02).
      *
      *保険組合せ番号
           03  RECE200-HKNCOMBI            PIC 9(04).
      *
      *実日数
           03  RECE200-JNISSU              PIC 9(03).
      *
      *診療区分別点数情報
           03  RECE200-SRYKBNINF           OCCURS  21.
      *基本点数
               05  RECE200-KHNTEN          PIC 9(08).
      *回数
               05  RECE200-KAISU           PIC 9(05).
      *点数
               05  RECE200-TENSU           PIC 9(08)   OCCURS  2.
      *
      *小計情報
           03  RECE200-SYOUKEIINF-AREA.
               05  RECE200-SYOUKEIINF      OCCURS  3.
      *小計点数
                   07  RECE200-SYOUKEI     PIC 9(08).
      *
      *合計情報
           03  RECE200-TOTALTENINF-AREA.
               05  RECE200-TOTALTENINF     OCCURS  4.
      *合計点数
                   07  RECE200-TOTALTEN    PIC 9(08).
      *
      *入院基本料種別情報
           03  RECE200-NYUSYUINF           OCCURS  20.
      *入院基本料種別
               05  RECE200-NYUSYUBETU      PIC X(20).
      *
      *入院基本料情報
           03  RECE200-NYUKHNINF           OCCURS  20.
      *入院基本料・加算
               05  RECE200-NYUKHNRYO       PIC 9(09).
      *入院日数
               05  RECE200-NYUKHNNISSU     PIC 9(02).
      *合計金額
               05  RECE200-NYUKHNKEI       PIC 9(09).
      *
      *入院基本料情報２
           03  RECE200-NYUKHNINF2.
      *合計金額２
               05  RECE200-NYUKHNKEI2      PIC S9(09).
      *
      *特定入院基本料情報
           03  RECE200-NYUTOKINF           OCCURS  20.
      *特定入院料・その他（点）
               05  RECE200-NYUTOKRYO       PIC 9(09).
      *特定入院料・その他（略称）
               05  RECE200-NYUTOKRYO-RYAKU PIC X(20).
      *入院日数
               05  RECE200-NYUTOKNISSU     PIC 9(02).
      *合計金額
               05  RECE200-NYUTOKKEI       PIC 9(09).
      *
      *特定入院基本料情報２
           03  RECE200-NYUTOKINF2.
      *合計金額２
               05  RECE200-NYUTOKKEI2      PIC S9(09).
      *
      *公害入院療養指導
           03  RECE200-KOGAISIDOINF        OCCURS   3.
      *基本料
               05  RECE200-KOGAISIDORYO    PIC 9(09).
      *日数
               05  RECE200-KOGAISIDONISSU  PIC 9(02).
      *合計金額
               05  RECE200-KOGAISIDOKEI    PIC 9(09).
      *
      *清浄空気室管理料
           03  RECE200-SEIKANINF.
      *基本料
               05  RECE200-SEIKANRYO       PIC 9(09).
      *日数
               05  RECE200-SEIKANNISSU     PIC 9(02).
      *合計金額
               05  RECE200-SEIKANKEI       PIC 9(09).
      *
      *食事種別情報‐基本
           03  RECE200-SHOKUJISYUBETU-KHN  PIC X(02).
      *
      *食事種別情報
           03  RECE200-SHOKUJISYUINF       OCCURS   3.
      *食事種別
               05  RECE200-SHOKUJISYUBETU  PIC X(04).
      *
      *食事基本料情報
           03  RECE200-SHOKUJIKHNINF       OCCURS   4.
      *食事基本料
               05  RECE200-SHOKUJIKHNRYO   PIC 9(09).
      *食事日数
               05  RECE200-SHOKUJIKHNNISSU PIC 9(02).
      *
      *合計日数（食事）
           03  RECE200-SHOKUJITOTALDAY     PIC 9(02).
      *
      *食事基本料の金額集計
           03  RECE200-SHOKUJIKHNRYO-KEI   PIC 9(09).
      *
      *他保険入院料算定済みフラグ
           03  RECE200-NYUINRYOFLG         PIC X(01).
      *
      *最終診療科
           03  RECE200-LASTSRYKA           PIC X(02).
      *最終病棟番号
           03  RECE200-LASTBTUNUM          PIC X(02).
      *最終病室番号
           03  RECE200-LASTBRMNUM          PIC X(08).
      *
      *入院年月日
           03  RECE200-NYUINYMD            PIC X(08).
      *
      *医療機関種別
           03  RECE200-HOSPSBT             PIC X(01).
      *病棟区分情報
           03  RECE200-BYOTOKBNINF         OCCURS  4.
      *病棟区分
               05  RECE200-BYOTOKBN        PIC X(02).
      *病棟種別情報
           03  RECE200-BYOTOSYUBETUINF     OCCURS 31.
      *病棟番号
               05  RECE200-BYOTONUM        PIC X(02).
      *病棟種別
               05  RECE200-BYOTOSYUBETU    PIC X(02).
      *
      *診療回数テーブル
           03  RECE200-DAY-AREA.
               05  RECE200-DAY             PIC 9(03)
                                           OCCURS  31.
      *
      *実日数テーブル情報
           03  RECE200-JNISSUTBLINF        OCCURS  31.
      *実日数テーブル
               05  RECE200-JNISSUTBL       PIC X(01).
      *
      *入院料情報取得フラグ
           03  RECE200-DOUJITSU-FLG        PIC 9(01).
      *入院料情報
           03  RECE200-DOUJITSU-AREA.
      *同日再入院（入院分）の保険組合せ
               05  RECE200-DOUJITSU-HKNCOMBI
                                          PIC 9(04)   OCCURS  31.
      *同日再入院（入院分）の食事開始時間区分
               05  RECE200-DOUJITSU-SKJKBN
                                          PIC X(01)   OCCURS  31.
      *入院区分
               05  RECE200-NYUINDAY-TBL.
                   07  RECE200-NYUINDAY-KBN
                                          PIC  X(01)  OCCURS  31.
      *
      *初診時間区分
           03  RECE200-SHOSHINTIME         PIC X(01).
      *外来迅速検体検査加算フラグ
           03  RECE200-KENTAIKASANFLG      PIC X(01).
      *コメントカウントフラグ
           03  RECE200-COMCNTFLG           PIC X(01).
      *病床数編集フラグ（特定疾患療養指導料等）
           03  RECE200-BEDSUFLG            PIC X(01).
      *特別な関係にある他院歴ありフラグ
           03  RECE200-TAINFLG             PIC X(01).
      *印刷順
           03  RECE200-PRTSEQ              PIC 9(03).
      *未請求フラグ
           03  RECE200-MISEIKYU-FLG        PIC X(02).
      *未訂正区分
           03  RECE200-TEISEIKBN           PIC X(01).
      *レセプト種別不明エラー情報
           03  RECE200-ERR-AREA.
               05  RECE200-ERR-ERRKBN      PIC 9(01).
               05  RECE200-ERR-DAY         PIC 9(02).
               05  RECE200-ERR-HKNCOMBINUM PIC 9(04).
               05  RECE200-ERR-TEKSTYMD    PIC 9(08).
               05  RECE200-ERR-TEKEDYMD    PIC 9(08).
      *
      *超重症児（者）入院診療加算等の算定フラグ
           03  RECE200-CYOJKSNFLG          PIC X(01).
      *
      *再診加算（入院）情報
           03  RECE200-NYUSAIINF.
               05  RECE200-NYUSAI-TENSU    PIC 9(08).
      *入院料日別診療区分情報
           03  RECE200-NYUDAYINF.
               05  RECE200-NYUDAY-SRYKBN   PIC X(02)  OCCURS 31.
      *
      *夜間看護体制特定日減算情報
           03  RECE200-KANGOGENINF.
               05  RECE200-KANGOGEN-SRYCD  PIC X(09)  OCCURS 31.
      *
      *妊婦フラグ
           03  RECE200-NINPUFLG            PIC 9(01).
      *
      *検査フラグ
           03  RECE200-KENSA-BNPFLG        PIC 9(01).
           03  RECE200-KENSA-NTPROFLG      PIC 9(01).
           03  RECE200-KENSA-ANPFLG        PIC 9(01).
      *
      *リハビリ情報
           03  RECE200-RIHAINF-AREA.
      *リハビリ算定テーブル２
               05  RECE200-RIHA2-DAY-AREA.
                 06  RECE200-RIHA2-SINDAI-AREA.
                   07  RECE200-RIHA2-SINDAI      PIC 9(01).
                   07  RECE200-RIHA2-SINYMD-OCC1.
                     08  RECE200-RIHA2-SINYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-SINYMD-OCC2.
                     08  RECE200-RIHA2-SINYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-KOKYUKI-AREA.
                   07  RECE200-RIHA2-KOKYUKI     PIC 9(01).
                   07  RECE200-RIHA2-KOKYMD-OCC1.
                     08  RECE200-RIHA2-KOKYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-KOKYMD-OCC2.
                     08  RECE200-RIHA2-KOKYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-NOUKE-AREA.
                   07  RECE200-RIHA2-NOUKE       PIC 9(01).
                   07  RECE200-RIHA2-NOUYMD-OCC1.
                     08  RECE200-RIHA2-NOUYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-NOUYMD-OCC2.
                     08  RECE200-RIHA2-NOUYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-UNDOKI-AREA.
                   07  RECE200-RIHA2-UNDOKI      PIC 9(01).
                   07  RECE200-RIHA2-UNDYMD-OCC1.
                     08  RECE200-RIHA2-UNDYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-UNDYMD-OCC2.
                     08  RECE200-RIHA2-UNDYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-NANBYO-AREA.
                   07  RECE200-RIHA2-NANBYO      PIC 9(01).
                   07  RECE200-RIHA2-NANYMD-OCC1.
                     08  RECE200-RIHA2-NANYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-NANYMD-OCC2.
                     08  RECE200-RIHA2-NANYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-SYOGAI-AREA.
                   07  RECE200-RIHA2-SYOGAI      PIC 9(01).
                   07  RECE200-RIHA2-SYOYMD-OCC1.
                     08  RECE200-RIHA2-SYOYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-SYOYMD-OCC2.
                     08  RECE200-RIHA2-SYOYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-SESSYOKU-AREA.
                   07  RECE200-RIHA2-SESSYOKU    PIC 9(01).
                   07  RECE200-RIHA2-SESYMD-OCC1.
                     08  RECE200-RIHA2-SESYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-SESYMD-OCC2.
                     08  RECE200-RIHA2-SESYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-HAIYO-AREA.
                   07  RECE200-RIHA2-HAIYO       PIC 9(01).
                   07  RECE200-RIHA2-HAIYMD-OCC1.
                     08  RECE200-RIHA2-HAIYMD1   PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-HAIYMD-OCC2.
                     08  RECE200-RIHA2-HAIYMD2   PIC 9(01)  OCCURS 31.
      *リハビリ加算算定テーブル２
               05  RECE200-RIHA2-DAY9-AREA.
                 06  RECE200-RIHA2-SINDAI9-AREA.
                   07  RECE200-RIHA2-SINDAI9     PIC 9(01).
                   07  RECE200-RIHA2-SIN9YMD-OCC1.
                     08  RECE200-RIHA2-SIN9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-SIN9YMD-OCC2.
                     08  RECE200-RIHA2-SIN9YMD2  PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-KOKYUKI9-AREA.
                   07  RECE200-RIHA2-KOKYUKI9    PIC 9(01).
                   07  RECE200-RIHA2-KOK9YMD-OCC1.
                     08  RECE200-RIHA2-KOK9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-KOK9YMD-OCC2.
                     08  RECE200-RIHA2-KOK9YMD2  PIC 9(01)  OCCURS 31.
      *
                 06  RECE200-RIHA2-HAIYO9-AREA.
                   07  RECE200-RIHA2-HAIYO9      PIC 9(01).
                   07  RECE200-RIHA2-HAI9YMD-OCC1.
                     08  RECE200-RIHA2-HAI9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE200-RIHA2-HAI9YMD-OCC2.
                     08  RECE200-RIHA2-HAI9YMD2  PIC 9(01)  OCCURS 31.
      *
      *排尿自立支援加算の該当月最初の算定日
           03  RECE200-HAIJSKYMD                 PIC X(08).
      *
      *予備
           03  RECE200-YOBIETC-AREA.
               05  RECE200-YOBIETCINF      OCCURS 10.
                   07  RECE200-YOBIETC     PIC 9(09).
      *
