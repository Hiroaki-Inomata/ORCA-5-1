$:.unshift(File.expand_path(File.join(File.dirname(__FILE__),"..")))
require "test/unit"
require "plugin/db"

class JMA::Plugin::DBTest < Test::Unit::TestCase
  include JMA::Plugin

  def setup
    ENV['LDDIRECTORY'] = './data/directory'
    @db = DB.new
    @db.clear
  end

  def teardown
  end

  def test_initialize
    assert_equal([],@db.list)
  end

  def test_insert_minimum_ok
    control = {:name => "foo", :version => "1.0", :vendor =>"test", :date => "20090101", :url => "file:///test"}
    assert_nothing_raised {
      @db.insert(control)
    }
    assert_equal(["foo"], @db.list.map{|c| c[:name]})
    control_new = @db.get_control(control[:name],control[:version])
    assert_equal(control[:name],control_new[:name])
    assert_equal(control[:version],control_new[:version])
  end


  def test_insert_ng_no_name
    control = {:name2 => "foo", :version => "1.0", :vendor =>"test", :date => "20090101", :url => "file:///test"}
    assert_nothing_raised {
      @db.insert(control)
    }
    assert_equal([], @db.list)
  end


  def test_insert_ng_no_version
    control = {:name => "foo", :version2 => "1.0", :vendor =>"test", :date => "20090101", :url => "file:///test"}
    assert_nothing_raised {
      @db.insert(control)
    }
    assert_equal([], @db.list)
  end

  def test_insert_full_ok
    control = YAML.load(<<-EOS)
    :name: foo
    :version: "1.0"
    :description: test plugin
    :vendor: test@example.com
    :date: "2009-01-01"
    :url: "file:///test"
    :install: "FALSE"
    :link: "FALSE"
    :available: "TRUE"
    EOS

    assert_nothing_raised {
      @db.insert(control)
    }
    assert_equal(["foo"], @db.list.map{|c| c[:name]})
    control_new = @db.get_control(control[:name],control[:version])
    assert_equal(control,control_new)
  end

  def test_link
    control = {:name => "foo", :version => "1.0", :vendor =>"test", :date => "20090101", :url => "file:///test"}
    assert_nothing_raised {
      @db.insert(control)
    }
    assert_nothing_raised {
      @db.link("foo","1.0")
    }
    assert_equal("TRUE",@db.get_control("foo","1.0")[:link])
  end

  def test_unlink
    control = {:name => "foo", :version => "1.0", :vendor =>"test", :date => "20090101", :url => "file:///test"}
    assert_nothing_raised {
      @db.insert(control)
    }
    assert_nothing_raised {
      @db.install("foo","1.0")
      @db.link("foo","1.0")
    }
    assert_nothing_raised {
      @db.unlink("foo","1.0")
    }
    assert_equal("FALSE",@db.get_control("foo","1.0")[:link])
  end

  def test_link?
    control = {:name => "foo", :version => "1.0", :vendor =>"test", :date => "20090101", :url => "file:///test"}
    assert_nothing_raised {
      @db.insert(control)
    }
    assert_equal(false,@db.link?("foo","1.0"))
    assert_nothing_raised {
      @db.link("foo","1.0")
    }
    assert_equal(true,@db.link?("foo","1.0"))
  end

  def test_install
    control = {:name => "foo", :version => "1.0", :vendor =>"test", :date => "20090101", :url => "file:///test"}
    assert_nothing_raised {
      @db.insert(control)
    }
    assert_nothing_raised {
      @db.install("foo","1.0")
    }
    assert_equal("TRUE",@db.get_control("foo","1.0")[:install])
  end

  def test_uninstall
    control = {:name => "foo", :version => "1.0", :vendor =>"test", :date => "20090101", :url => "file:///test"}
    assert_nothing_raised {
      @db.insert(control)
    }
    assert_nothing_raised {
      @db.install("foo","1.0")
    }
    assert_nothing_raised {
      @db.uninstall("foo","1.0")
    }
    assert_equal("FALSE",@db.get_control("foo","1.0")[:install])
  end

  def test_install?
    control = {:name => "foo", :version => "1.0", :vendor =>"test", :date => "20090101", :url => "file:///test"}
    assert_nothing_raised {
      @db.insert(control)
    }
    assert_equal(false,@db.install?("foo","1.0"))
    assert_nothing_raised {
      @db.install("foo","1.0")
    }
    assert_equal(true,@db.install?("foo","1.0"))
  end

end
