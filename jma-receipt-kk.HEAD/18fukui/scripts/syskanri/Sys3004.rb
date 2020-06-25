#!/usr/bin/ruby

$:.unshift("/usr/lib/jma-receipt/scripts/daily")

require	"apslib.rb"

#################################################
#
#  �����ƥ�����ޥ�����3004�פ˥ǡ��������뤫
#���ʰ��٤⻲�Ȥ��Ƥ��ʤ����Τߡ��ǡ�����̵����
#  ����  ��1=���ŵ����ֹ桡2=����������
#  ����͡�0=�ǡ���ͭ�ꡡ1=�ǡ���̵��
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
#  �����ƥ�����ޥ�����3004�פΥ�����ɣĽ�ʣ�����å�
#
#  ����  ��1=���ŵ����ֹ桡2=���������ɡ�3=�ǡ���  4=����ǯ����  5=��λǯ����
#  ����͡�0=��ʣ̵����̤��Ͽ�ˡ�1=��ʣͭ�����Ͽ�Ѥߡ�
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
#  Ʊ��shellid����Ͽ����Ƥ���ʤ��ɲä��ʤ�
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
#  �����ƥ�����ޥ����λ���
#
#  ����  ��1=���ŵ����ֹ桡2=���������ɡ�3=�ǡ���  4=����ǯ����  5=��λǯ����
#  ����͡������Ƥ����ʬ������
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
#   �����Ƥ��뤫
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
#  �����ƥ�����ޥ�����3004�פإǡ�����Ͽ���ɲá�
#
#  ����  ��1=���ŵ����ֹ桡2=���������ɡ�3=��ʬ�����ɡ�4=�ǡ���
#  ����͡�0=���0�ʳ��ᥨ�顼
#
#################################################
def sys3004_insert(hospnum,kanricd,kbncd,line)
  sys3004 = PandaTable.new(@db,"tbl_syskanri")
  t = Time.new
  creymd = t.strftime("%Y%m%d")
  crehms = t.strftime("%H%M%S")
# ��Ͽ����ǡ����β��ԥ����ɽ���
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
#  �����ƥ�����ޥ�����3004�פإǡ�����Ͽ�ʹ�����
#
#  ����  ��1=���ŵ����ֹ桡2=���������ɡ�3=��ʬ�����ɡ�4=�ǡ���
#          5=����ǯ����  6=��λǯ����
#  ����͡�0=���0�ʳ��ᥨ�顼
#
#################################################
def sys3004_update(hospnum,kanricd,kbncd,line,stymd,edymd)
  sys3004 = PandaTable.new(@db,"tbl_syskanri")
  t = Time.new
  creymd = t.strftime("%Y%m%d")
  crehms = t.strftime("%H%M%S")
# ��Ͽ����ǡ����β��ԥ����ɽ���
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
#  �����ƥ�����ޥ�����3004�פإǡ�����Ͽ���ɲá�
#
#  ����  ��1=���ŵ����ֹ桡2=����������
#
#################################################
def sys3004_insert_main(hospnum,kanricd)
  count = 0
  f = open("sys3004.data")
  while line = f.gets
#   ��Ͽ�¹�
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
#  �����ƥ�����ޥ�����3004�פκǿ�ͭ�����¼���
#
#  ������1=���ŵ����ֹ�  2=����������
#  ����͡�1=ͭ������ǯ����  2=ͭ����λǯ����
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
#  �����ƥ�����ޥ�����3004�פإǡ�����Ͽ�ʹ�����
#
#  ����  ��1=���ŵ����ֹ桡2=���������ɡ�3=��ʬ������
#
#################################################
def sys3004_update_main(hospnum,kanricd,kbncd)
  upcheck = 0
  stymd = 0
  edymd = 0
# �ǿ���ͭ�����¼���
  stymd,edymd = kigen_get(hospnum,kanricd)

  f = open("sys3004.data")
  while line = f.gets
#   shellid�ν�ʣ�����å�
    result = sys3004_check(hospnum,kanricd,line,stymd,edymd)
    if result == 0
#   ��Ͽ�������Ϥ��뤫
      result = sys3004_select(hospnum,kanricd,line,stymd,edymd)
      kbncd = result
      if result != 1
#   ��Ͽ�¹�
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
#   �ѹ�̵�����ä���
  if  upcheck == 0
    $stderr.printf("�ѹ��Ϥ���ޤ���Ǥ����� \n")
  end
  
  f.close
end

#################################################
#
# <  Main  >
#
# ������1=���ŵ����ֹ�
#
#################################################
hospnum = ARGV[0]

@db = PandaDB.new
@db.execFunction("DBOPEN")
@db.execFunction("DBSTART")

$stderr.printf("\nSys3004 start\n\n")

#  ����ʤ����֤��ɤ����Υ����å�����Ͽ̵ͭ�����å���
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
