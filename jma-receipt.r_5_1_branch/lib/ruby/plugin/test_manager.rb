$:.unshift(File.expand_path(File.join(File.dirname(__FILE__),"..")))
require "test/unit"
require "pp"
require "webrick"
require "plugin"
require "plugin/manager"

class JMA::Plugin::ManagerTest < Test::Unit::TestCase
  include JMA::Plugin

  JMA::Plugin::TAR_COMMAND = "tar"

  def setup
    ENV['LDDIRECTORY'] = './data/directory'

    `psql testplugin -c "delete from tbl_plugin;"`

    `gpg --import --allow-secret-key-import data/test.gpg`

    @root = File.expand_path(File.join(File.dirname(__FILE__),"store"))
    FileUtils.rm_rf(@root) if File.exist?(@root)
    @linkprefix = (File.join(@root,"linkprefix"))
    FileUtils.rm_rf(@linkprefix) if File.exist?(@linkprefix)
    @datadir = File.expand_path(File.join(File.dirname(__FILE__),"data"))

    @config = {}
    @config[:root] = @root
    @config[:list] = []
    @config[:list] << "http://localhost:3000/list1.yml"
    @config[:list] << "http://localhost:3000/list2.yml"
    @config[:linkprefix] = @linkprefix
    @config[:verify] = true
    @manager = Manager.new(@config)
    
    @httpd = WEBrick::HTTPServer.new(
      :Logger       => @logger,
      :ServerType   => Thread,
      :DocumentRoot => @datadir,
      :Port         => 3000
    )
    @httpd.start
  end

  def teardown
    @httpd.shutdown
  end

  def test_initialize
    assert_equal([], @manager.list)
  end

  def test_update
    assert_nothing_raised{
      @manager.update
    }
    assert_equal(
      [["bar","1.0"],["foo","1.0"],["foo","1.1"]], 
      @manager.list.sort!{|a,b| a[:name] <=> b[:name]}.map{|c| [c[:name],c[:version]] }
    )
  end

  def test_check
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.install("bar","1.0")
      @manager.link("bar","1.0")
    }
    assert_equal([[],[]],@manager.check)
  end

  def test_check_install
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.install("bar","1.0")
      @manager.link("bar","1.0")
    }
    FileUtils.rm_rf(File.join(@root,"foo-1.1"))
    ie,le = @manager.check
    ie2 = ie.map do |c| "#{c[:name]}-#{c[:version]}" end
    assert_equal(["foo-1.1"],ie2)
    assert(le.empty?)
  end

  def test_check_link
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.link("foo","1.1")
      @manager.install("bar","1.0")
      @manager.link("bar","1.0")
    }
    FileUtils.rm_rf(File.join(@root,"foo-1.1",".linked"))
    ie,le = @manager.check
    assert(ie.empty?)
    le2 = le.map do |c| "#{c[:name]}-#{c[:version]}" end
    assert_equal(["foo-1.1"],le2)
  end

  def test_restore_unnecessary
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.link("foo","1.1")
      @manager.install("bar","1.0")
      @manager.link("bar","1.0")
    }
    assert_nothing_raised{
      @manager.restore
    }
    assert_equal(true,Dir.exist?(File.join(@root,"foo-1.1")))
    assert_equal(true,File.exist?(File.join(@root,"foo-1.1",".linked")))
    assert_equal(true,Dir.exist?(File.join(@root,"bar-1.0")))
    assert_equal(true,File.exist?(File.join(@root,"bar-1.0",".linked")))
  end

  def test_restore
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.link("foo","1.1")
      @manager.install("bar","1.0")
      @manager.link("bar","1.0")
    }
    FileUtils.rm_rf(File.join(@root,"foo-1.1"))
    FileUtils.rm_rf(File.join(@root,"bar-1.0",".linked"))
    assert_nothing_raised{
      @manager.restore
    }
    assert_equal(true,Dir.exist?(File.join(@root,"foo-1.1")))
    assert_equal(true,File.exist?(File.join(@root,"foo-1.1",".linked")))
    assert_equal(true,Dir.exist?(File.join(@root,"bar-1.0")))
    assert_equal(true,File.exist?(File.join(@root,"bar-1.0",".linked")))
  end

  def test_force_restore_unnecessary
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.link("foo","1.1")
      @manager.install("bar","1.0")
      @manager.link("bar","1.0")
    }
    assert_nothing_raised{
      @manager.force_restore
    }
    assert_equal(true,Dir.exist?(File.join(@root,"foo-1.1")))
    assert_equal(true,File.exist?(File.join(@root,"foo-1.1",".linked")))
    assert_equal(true,Dir.exist?(File.join(@root,"bar-1.0")))
    assert_equal(true,File.exist?(File.join(@root,"bar-1.0",".linked")))
  end

  def test_force_restore
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.link("foo","1.1")
      @manager.install("bar","1.0")
      @manager.link("bar","1.0")
    }
    FileUtils.rm_rf(File.join(@root,"foo-1.1"))
    FileUtils.rm_rf(File.join(@root,"bar-1.0",".linked"))
    assert_nothing_raised{
      @manager.force_restore
    }
    assert_equal(true,Dir.exist?(File.join(@root,"foo-1.1")))
    assert_equal(true,File.exist?(File.join(@root,"foo-1.1",".linked")))
    assert_equal(true,Dir.exist?(File.join(@root,"bar-1.0")))
    assert_equal(true,File.exist?(File.join(@root,"bar-1.0",".linked")))
  end

  def test_install
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.install("bar","1.0")
    }
    assert_equal(
      [["bar","1.0"],["foo","1.1"]], 
      @manager.list("install").map do |c| [c[:name],c[:version]] end
    )
  end

  def test_install2
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.link("foo","1.1")
      @manager.install("bar","1.0")
    }

    @config[:list] << "http://localhost:3000/list3.yml"
    @manager = Manager.new(@config)
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.2")
    }
    assert_equal(
      [["bar","1.0"],["foo","1.2"]], 
      @manager.list("install").map do |c| [c[:name],c[:version]] end
    )
    assert(@manager.info("foo","1.2")[:link])
  end

  def test_install_no_verify
    @config[:no_verify] = true
    @manager = Manager.new(@config)
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.install("bar","1.0")
    }
    assert_equal(
      [["bar","1.0"],["foo","1.1"]], 
      @manager.list('install').map{|c| [c[:name],c[:version]] }
    )
  end

  def test_install_ng
    assert_nothing_raised{
      @manager.update
    }
    assert_raise(RuntimeError){
      @manager.install("foo","1.2")
    }
  end

  def test_uninstall
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.link("foo","1.1")
      @manager.install("bar","1.0")
    }
    assert_nothing_raised{
      @manager.uninstall("foo","1.1")
    }
    assert_equal(
      [["bar","1.0"]],
      @manager.list('install').map{|c| [c[:name],c[:version]] }
    )
    assert_equal(false,File.exist?(File.join(@linkprefix,"foo-1.1.txt")))
  end

  def test_uninstall_ng
    assert_raise(RuntimeError){
      @manager.uninstall("foo","1.1")
    }
  end

  def test_uninstall_ng2
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("bar","1.0")
    }
    assert_raise(RuntimeError){
      @manager.uninstall("foo","1.1")
    }
    assert_equal(
      [["bar","1.0"]],
      @manager.list('install').map{|c| [c[:name],c[:version]] }
    )
  end

  def test_link
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.install("bar","1.0")
    }
    assert_nothing_raised{
      @manager.link("foo","1.1")
    }
    assert_equal(
      [["foo","1.1"]], 
      @manager.list('enable').map{|c| [c[:name],c[:version]] }
    )
    assert(@manager.list('enable').detect{|c| c[:name]=="foo" && c[:version]=="1.1"})
    assert_equal(true,File.exist?(File.join(@linkprefix,"foo-1.1.txt")))
  end

  def test_link_ng
    assert_nothing_raised{
      @manager.update
    }
    assert_raise(RuntimeError){
      @manager.link("foo","1.1")
    }
    assert_equal(false,File.exist?(File.join(@linkprefix,"foo-1.1.txt")))
  end

  def test_link_ng2
    assert_raise(RuntimeError){
      @manager.link("foo","1.1")
    }
    assert_equal(false,File.exist?(File.join(@linkprefix,"foo-1.1.txt")))
  end

  def test_unlink
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.install("bar","1.0")
    }
    assert_nothing_raised{
      @manager.link("foo","1.1")
    }
    assert_equal(true,File.exist?(File.join(@linkprefix,"foo-1.1.txt")))
    assert_nothing_raised{
      @manager.unlink("foo","1.1")
    }
    assert(@manager.list.detect{|c| c[:link]=="FALSE" && c[:name]=="foo" && c[:version]=="1.1"})
    assert_equal(false,File.exist?(File.join(@linkprefix,"foo-1.1.txt")))
  end

  def test_unlink_ng
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.install("bar","1.0")
    }
    assert_nothing_raised{
#      @manager.link("foo","1.1")
    }
    assert_raise(RuntimeError){
      @manager.unlink("foo","1.1")
    }
  end

  def test_unlink_ng2
    assert_nothing_raised{
      @manager.update
    }
    assert_raise(RuntimeError){
      @manager.unlink("foo","1.1")
    }
    assert_equal(false,File.exist?(File.join(@linkprefix,"foo-1.1.txt")))
  end

  def test_unlink_ng3
    assert_raise(RuntimeError){
      @manager.unlink("foo","1.1")
    }
    assert_equal(false,File.exist?(File.join(@linkprefix,"foo-1.1.txt")))
  end

  def test_list
    list = []
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      list = @manager.list
    }
	pp list
    assert_equal(3,list.size)
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.install("bar","1.0")
      @manager.link("foo","1.1")
    }
    assert_nothing_raised{
      list = @manager.list(true)
    }
    assert_equal(3,list.size)
    assert_equal(["bar","foo","foo"],list.map{|c| c[:name]}.sort)
    assert_nothing_raised{
      list = @manager.list("enable")
    }
    assert_equal(1,list.size)
    assert_equal(["foo","1.1"],[list[0][:name],list[0][:version]])
  end

  def test_upgrade
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.link("foo","1.1")
      @manager.install("bar","1.0")
    }

    @config[:list] << "http://localhost:3000/list3.yml"
    @manager = Manager.new(@config)
    assert_nothing_raised{
      @manager.update
    }
    list = []
    assert_nothing_raised{
      list = @manager.list_upgrade
    }
    assert_equal(["foo","1.2"],[list[0][:new][:name],list[0][:new][:version]])
    assert_nothing_raised{
      @manager.upgrade
    }
    assert_equal(
      [['bar','1.0'],['foo','1.2']], 
      @manager.list('install').map do |c| [c[:name],c[:version]] end
    )
    assert_equal(true,File.exist?(File.join(@linkprefix,'foo-1.2.txt')))
  end

  def test_search
    @config[:list] = []
    @config[:list] << "http://localhost:3000/list1.yml"
    @manager = Manager.new(@config)
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.0")
    }

    @config[:list] = []
    @config[:list] << "http://localhost:3000/list1.yml"
    @config[:list] << "http://localhost:3000/list2.yml"
    @config[:list] << "http://localhost:3000/list3.yml"
    @manager = Manager.new(@config)
    assert_nothing_raised{
      @manager.update
    }

    assert_equal(
      [["foo","1.0"],["foo","1.1"],["foo","1.2"]], 
      @manager.search("fo").sort!{|a,b| a[:version] <=> b[:version]}.map{|c| [c[:name],c[:version]] }
    )
    assert_equal(
      [["bar","1.0"]], 
      @manager.search("r").sort!{|a,b| a[:name] <=> b[:name]}.map{|c| [c[:name],c[:version]] }
    )
  end

  def test_clean_cache
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.link("foo","1.1")
      @manager.install("bar","1.0")
    }
    p Dir.glob(File.join(@config[:root],"cache","*"))
    assert_equal(
      ["bar-1.0.jpp","bar-1.0.jpp.asc","foo-1.1.jpp","foo-1.1.jpp.asc"], 
      Dir.glob(File.join(@config[:root],"cache","*")).map{|f| File.basename(f)}.sort)
    assert_nothing_raised{
      @manager.clean_cache
    }
    assert_equal([], Dir.glob(File.join(@config[:root],"cache","*")))
  end

  def test_info
    assert_nothing_raised{
      @manager.update
    }
    assert_nothing_raised{
      @manager.install("foo","1.1")
      @manager.link("foo","1.1")
    }
    assert_nothing_raised{
      @manager.info("foo","1.1")
      @manager.info("bar","1.0")
    }
  end
end
