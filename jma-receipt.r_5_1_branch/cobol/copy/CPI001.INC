      ************************************************************
      *    入院患者照会ビュー     COPY (CPI001.INC)              *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  I001-REC.
      *医療機関ＩＤ
           03  I001-HOSPNUM                PIC  9(02).
      *患者ＩＤ
           03  I001-PTID                   PIC  9(10).
      *入院履歴番号
           03  I001-RRKNUM                 PIC  9(03).
      *入院履歴枝番号
           03  I001-RRKEDANUM              PIC  9(03).
      *入院履歴枝番号（歴内最大番号）
           03  I001-RRKEDANUM-MAX          PIC  9(03).
      *患者番号
           03  I001-PTNUM                  PIC  X(20).
      *カナ氏名
           03  I001-KANANAME               PIC  X(100).
      *漢字氏名
           03  I001-NAME                   PIC  X(100).
      *性別
           03  I001-SEX                    PIC  X(01).
      *生年月日
           03  I001-BIRTHDAY               PIC  X(08).
      *自宅−郵便番号
           03  I001-HOME-POST              PIC  X(07).
      *自宅−住所
           03  I001-HOME-ADRS              PIC  X(200).
      *自宅−番地方書
           03  I001-HOME-BANTI             PIC  X(200).
      *死亡区分
           03  I001-DEATHKBN               PIC  X(01).
      *病棟番号
           03  I001-BTUNUM                 PIC  X(02).
      *病室番号
           03  I001-BRMNUM                 PIC  X(08).
      *入院科
           03  I001-NYUINKA                PIC  X(02).
      *入院日
           03  I001-NYUINYMD               PIC  X(08).
      *退院日
           03  I001-TAIINYMD               PIC  X(08).
      *退院事由
           03  I001-TAIINCD                PIC  X(02).
      *転入日
           03  I001-TENNYUYMD              PIC  X(08).
      *転出日
           03  I001-TENSTUYMD              PIC  X(08).
      *テスト患者番号区分
           03  I001-TSTPTNUMKBN            PIC  X(01).
      *取得件数
           03  I001-COUNT                  PIC  9(10).
      *フリーＳＱＬ文（WHERE句）
           03  I001-SQL                    PIC  X(15000).
