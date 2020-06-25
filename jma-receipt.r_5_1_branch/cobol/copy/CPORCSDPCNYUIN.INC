      ************************************************************
      *    入院ＤＰＣ取得パラメタ　　　COPY (CPORCSDPCNYUIN.INC)
      *                                使用プログラム (ORCSDPCNYUIN)
      *    設定項目
      *      医療機関ＩＤ
      *      患者ＩＤ
      *      算定年月
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.00.01    NACL-小豆沢  xx/xx/xx  グループ診療対応
      ************************************************************
       01  ORCSDPCNYUIN-AREA.
      *
      *医療機関ＩＤ
           03  LNK-DPCNYUIN-HOSPNUM                PIC  9(02).
      *患者ＩＤ
           03  LNK-DPCNYUIN-PTID                   PIC  9(10).
      *算定年月
           03  LNK-DPCNYUIN-SANTEIYM               PIC  X(06).
      *保険区分（１：社保　２：国保　３：広域連合）
           03  LNK-DPCNYUIN-HKNKBN                 PIC  X(01).
      *対象保険組合せ番号
           03  LNK-DPCNYUIN-SET-HKNCOMBI           PIC  9(04) OCCURS 10.
      *返却項目
           03  LNK-DPCNYUIN-HENKYAKU-AREA.
      *入院返却テーブル（添字１：入院料　添字２：特定入院料　３：食事
      *                  添字４：生活療養（食事療養）
      *                  添字５：生活療養（環境療養））
             04  LNK-DPCNYUIN-HENKYAKU-TABLE       OCCURS  5.
      *入院料返却テーブル
               05  LNK-DPCNYUIN-HENKYAKU-TBL       OCCURS  40.
      *保険組合せ
                   07  LNK-DPCNYUIN-HKNCOMBI       PIC  9(04).
      *病棟番号
                   07  LNK-DPCNYUIN-BTUNUM         PIC  9(02).
      *病棟区分
                   07  LNK-DPCNYUIN-BTUKBN         PIC  X(02).
      *診療科
                   07  LNK-DPCNYUIN-SRYKA          PIC  X(02).
      *ドクターコード
                   07  LNK-DPCNYUIN-DRCD           PIC  X(05).
      *病棟番号２（同日再入院用）
                   07  LNK-DPCNYUIN-BTUNUM2        PIC  9(02).
      *病棟区分２（同日再入院用）
                   07  LNK-DPCNYUIN-BTUKBN2        PIC  X(02).
      *診療科２（同日再入院用）
                   07  LNK-DPCNYUIN-SRYKA2         PIC  X(02).
      *ドクターコード２（同日再入院用）
                   07  LNK-DPCNYUIN-DRCD2          PIC  X(05).
      *明細数
                   07  LNK-DPCNYUIN-MEISAISU       PIC  9(03).
      *レセ電記録エリア
                   07  LNK-DPCNYUIN-NYUIN          OCCURS 100.
      *レコード識別
                       09  LNK-DPCNYUIN-RECSKB     PIC  X(02).
      *診療識別
                       09  LNK-DPCNYUIN-SRYSKB     PIC  X(02).
      *診療行為コード
                       09  LNK-DPCNYUIN-SRYCD      PIC  X(09).
      *減算端数処理対象
                       09  LNK-DPCNYUIN-HASUU      PIC  X(01).
      *選定入院外泊区分
                       09  LNK-DPCNYUIN-SENTEI-GAIHAKU
                                                   PIC 9(01).
      *数量
                       09  LNK-DPCNYUIN-SURYO      PIC  9(01).
      *点数
                       09  LNK-DPCNYUIN-TEN        PIC  9(07).
      *回数
                       09  LNK-DPCNYUIN-KAISU      PIC  9(02).
      *コメント記録データ
                       09  LNK-DPCNYUIN-COMMENT    PIC  X(76).
      *同日再入院区分
                       09  LNK-DPCNYUIN-SAINYUINKBN PIC X(01).
      *対象日
                   07  LNK-DPCNYUIN-DD             PIC  9(02).
      *
      *復帰コード（１：算定年月エラー　２：患者検索エラー
      *            ４：入院会計なし　　５：保険組合せ取得エラー）
           03  LNK-DPCNYUIN-RC                     PIC S9(9) BINARY.
      *
