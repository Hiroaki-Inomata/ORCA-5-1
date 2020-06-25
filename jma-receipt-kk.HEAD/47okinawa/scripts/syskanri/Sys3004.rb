#!/usr/bin/ruby

$:.unshift("/usr/lib/jma-receipt/scripts/daily")

require	"apslib.rb"

#################################################
#
#  システム管理マスタ「3004」にデータがあるか
#　（一度も参照していない場合のみ、データが無い）
#  引数  　1=医療機関番号　2=管理コード
#  戻り値　0=データ有り　1=データ無し
#
#################################################
def sys3004_data_check(hospnum,kanricd)
  sys3004 = PandaTable.new(@db,"tbl_syskanri")
  sys3004["tbl_syskanri.HOSPNUM"] = hospnum
  sys3004["tbl_syskanri.KANRICD"] = kanricd
  if sys3004.execFunction("DBSELECT","key5") == 0
    if  sys3004.execFunction("DBFETCH","key5") == 0
      rc = 0
    else
      rc = 1
    end
    sys3004.execFunction("DBCLOSECURSOR","key5")
  else
    rc = 1
    $stderr.printf("dbselect error .\n")
  end
  return rc
end

#################################################
#
#  システム管理マスタ「3004」のシェルＩＤ重複チェック
#
#  引数  　1=医療機関番号　2=管理コード　3=データ  4=開始年月日  5=終了年月日
#  戻り値　0=重複無し（未登録）　1=重複有り（登録済み）
#
#################################################
def sys3004_check(hospnum,kanricd,line,stymd,edymd)
  rc = 0
  sys3004 = PandaTable.new(@db,"tbl_syskanri")
  sys3004["tbl_syskanri.HOSPNUM"] = hospnum
  sys3004["tbl_syskanri.KANRICD"] = kanricd
  sys3004["tbl_syskanri.STYUKYMD"] = stymd
  sys3004["tbl_syskanri.EDYUKYMD"] = edymd
  if sys3004.execFunction("DBSELECT","key2") == 0
    while  sys3004.execFunction("DBFETCH","key2") == 0
#  同じshellidが登録されているなら追加しない
      if  sys3004["tbl_syskanri.KANRITBL"][0,20] == line[0,20]
        rc = 1
        $stderr.printf("already set [ %s ].\n",line[0.20])
        break
      end
    end
    sys3004.execFunction("DBCLOSECURSOR","key2")
  else
    rc = 1
    $stderr.printf("dbselect error .\n")
  end
  return rc
end

#################################################
#
#  システム管理マスタの参照
#
#  引数  　1=医療機関番号　2=管理コード　3=データ  4=開始年月日  5=終了年月日
#  戻り値　空いている区分コード
#
#################################################
def sys3004_select(hospnum,kanricd,line,stymd,edymd)
  rc = 1
  sys3004 = PandaTable.new(@db,"tbl_syskanri")
  sys3004["tbl_syskanri.HOSPNUM"] = hospnum
  sys3004["tbl_syskanri.KANRICD"] = kanricd
  sys3004["tbl_syskanri.STYUKYMD"] = stymd
  sys3004["tbl_syskanri.EDYUKYMD"] = edymd
  if sys3004.execFunction("DBSELECT","key2") == 0
    while  sys3004.execFunction("DBFETCH","key2") == 0
#   空いているか
      if  sys3004["tbl_syskanri.KANRITBL"][0,60] == "                                                            "
        rc = sys3004["tbl_syskanri.KBNCD"]
        $stderr.printf("space ok [ %s ] .\n",sys3004["tbl_syskanri.KBNCD"])
        break
      end
    end
    sys3004.execFunction("DBCLOSECURSOR","key2")
  else
    $stderr.printf("dbselect error .\n")
  end
  return rc
end

#################################################
#
#  システム管理マスタ「3004」へデータ登録（追加）
#
#  引数  　1=医療機関番号　2=管理コード　3=区分コード　4=データ
#  戻り値　0=正常　0以外＝エラー
#
#################################################
def sys3004_insert(hospnum,kanricd,kbncd,line)
  sys3004 = PandaTable.new(@db,"tbl_syskanri")
  t = Time.new
  creymd = t.strftime("%Y%m%d")
  crehms = t.strftime("%H%M%S")
# 登録するデータの改行コード除去
  line.chomp!
  sys3004["tbl_syskanri.HOSPNUM"] = hospnum
  sys3004["tbl_syskanri.KANRICD"] = kanricd
  sys3004["tbl_syskanri.KBNCD"] = kbncd
  sys3004["tbl_syskanri.STYUKYMD"] = "00000000"
  sys3004["tbl_syskanri.EDYUKYMD"] = "99999999"
  sys3004["tbl_syskanri.KANRITBL"] = line
  sys3004["tbl_syskanri.UPHMS"] = "     0"
  rc = sys3004.execFunction("DBINSERT","key")
  return rc
end

#################################################
#
#  システム管理マスタ「3004」へデータ登録（更新）
#
#  引数  　1=医療機関番号　2=管理コード　3=区分コード　4=データ
#          5=開始年月日  6=終了年月日
#  戻り値　0=正常　0以外＝エラー
#
#################################################
def sys3004_update(hospnum,kanricd,kbncd,line,stymd,edymd)
  sys3004 = PandaTable.new(@db,"tbl_syskanri")
  t = Time.new
  creymd = t.strftime("%Y%m%d")
  crehms = t.strftime("%H%M%S")
# 登録するデータの改行コード除去
  line.chomp!
  sys3004["tbl_syskanri.HOSPNUM"] = hospnum
  sys3004["tbl_syskanri.KANRICD"] = kanricd
  sys3004["tbl_syskanri.KBNCD"] = kbncd
  sys3004["tbl_syskanri.STYUKYMD"] = stymd
  sys3004["tbl_syskanri.EDYUKYMD"] = edymd
  sys3004["tbl_syskanri.KANRITBL"] = line
  sys3004["tbl_syskanri.UPHMS"] = "     0"
  rc = sys3004.execFunction("DBUPDATE","key")
  return rc
end

#################################################
#
#  システム管理マスタ「3004」へデータ登録（追加）
#
#  引数  　1=医療機関番号　2=管理コード
#
#################################################
def sys3004_insert_main(hospnum,kanricd)
  count = 0
  f = open("sys3004.data")
  while line = f.gets
#   登録実行
    count = count + 1
    if  count > 9
      scount = "00"
      scount << count.to_s
    else
      scount = "000"
      scount << count.to_s
    end
    $stderr.printf("scount [ %s ] .\n",scount)
    result = sys3004_insert(hospnum,kanricd,scount,line)
    if result == 0
      $stderr.printf("sys3004 succeeded.\n")
    else
      $stderr.printf("sys3004 failed.\n")
    end
  end
  f.close
end

#################################################
#
#  システム管理マスタ「3004」の最新有効期限取得
#
#  引数　1=医療機関番号  2=管理コード
#  戻り値　1=有効開始年月日  2=有効終了年月日
#
#################################################
def kigen_get(hospnum,kanricd)
  stymd = 0
  edymd = 0
  sys3004 = PandaTable.new(@db,"tbl_syskanri")
  sys3004["tbl_syskanri.HOSPNUM"] = hospnum
  sys3004["tbl_syskanri.KANRICD"] = kanricd
  sys3004["tbl_syskanri.KBNCD"] = "0001"
  if sys3004.execFunction("DBSELECT","key8") == 0
    if  sys3004.execFunction("DBFETCH","key8") == 0
      stymd = sys3004["tbl_syskanri.STYUKYMD"]
      edymd = sys3004["tbl_syskanri.EDYUKYMD"]
    else
      $stderr.printf("dbselect error .\n")
    end
    sys3004.execFunction("DBCLOSECURSOR","key8")
  else
    $stderr.printf("dbselect error .\n")
  end
  return stymd,edymd
end

#################################################
#
#  システム管理マスタ「3004」へデータ登録（更新）
#
#  引数  　1=医療機関番号　2=管理コード　3=区分コード
#
#################################################
def sys3004_update_main(hospnum,kanricd,kbncd)
  upcheck = 0
  stymd = 0
  edymd = 0
# 最新の有効期限取得
  stymd,edymd = kigen_get(hospnum,kanricd)

  f = open("sys3004.data")
  while line = f.gets
#   shellidの重複チェック
    result = sys3004_check(hospnum,kanricd,line,stymd,edymd)
    if result == 0
#   登録する空欄はあるか
      result = sys3004_select(hospnum,kanricd,line,stymd,edymd)
      kbncd = result
      if result != 1
#   登録実行
        result = sys3004_update(hospnum,kanricd,kbncd,line,stymd,edymd)
        if result == 0
          $stderr.printf("sys3004 succeeded.\n")
          upcheck = 1
        else
          $stderr.printf("sys3004 failed.\n")
        end
      else
        $stderr.printf("sys3004 non space [ %s ].\n",kbncd)
      end
    else
      $stderr.printf("sys3004 already set [ %s ].\n",line[0,20])
    end
  end
#   変更無しだったか
  if  upcheck == 0
    $stderr.printf("変更はありませんでした。 \n")
  end
  
  f.close
end

#################################################
#
# <  Main  >
#
# 引数　1=医療機関番号
#
#################################################
hospnum = ARGV[0]

@db = PandaDB.new
@db.execFunction("DBOPEN")
@db.execFunction("DBSTART")

$stderr.printf("\nSys3004 start\n\n")

#  何もない状態かどうかのチェック（登録有無チェック）
result = sys3004_data_check(hospnum,"3004")

if result == 1
#  insert
  sys3004_insert_main(hospnum,"3004")
else
#  update
  sys3004_update_main(hospnum,"3004","%")
end

@db.execFunction("DBCOMMIT")
@db.execFunction("DBDISCONNECT")

$stderr.printf("\nSys3004 end\n")
