#!/usr/bin/ruby

require 'yaml'
require 'logger'
require 'fileutils'
require 'optparse'
require 'kconv'

def parse_option(argv0, argv)
  spec = [
    ["-c", "--config=VAL",      "config file",    :config],
    ["-U", "--url=VAL",         "base url",       :baseurl],
    ["-D", "--dist=VAL",        "dist dir",       :distdir],
    ["-l", "--list=VAL",        "list",           :list],
    ["-u", "--user=VAL",        "key user",       :keyuser],
    ["-p", "--pass=VAL",        "pass phrase",    :passphrase],
    ["-L", "--log=VAL",         "log",            :log],
  ]
  option = {}
  parser = OptionParser.new{|opts|
    opts.banner = "Usage: #{File.basename(argv0)} [option]\n"
    opts.on_head("options:")
    spec.each{|short, long, desc, key|
      opts.on(short, long, desc){|arg|
        option[key] = arg
      }
    }
    opts.on("-d", "--debug", "debug mode") {
      option[:debug] = true
    }
    opts.on_tail('--help', 'show this message'){
      raise "help specified"
    }
  }
  begin
    parser.parse!(argv)
  rescue RuntimeError => ex
    puts parser
    exit 1
  end
  config = YAML.load(File.read(option[:config])) rescue {}
  log = Logger.new(option[:log] || STDERR)
  log.level = option[:debug] ? Logger::DEBUG : Logger::INFO
  config[:log] = log
  config[:baseurl] = option[:baseurl] if option[:baseurl]
  config[:distdir] = option[:distdir] if option[:distdir]
  config[:list] = option[:list] if option[:list]
  config[:keyuser] = option[:keyuser] if option[:keyuser]
  config[:passphrase] = option[:passphrase] if option[:passphrase]
  return config
end

def check_control(c)
  raise "invalid control: no name" unless c[:name] 
  raise "invalid control: no version" unless c[:version] 
  raise "invalid control: no vendor" unless c[:vendor]
end

def clean
  # dist clean
  FileUtils.rm_rf(@opts[:distdir])
  FileUtils.mkdir_p(@opts[:distdir])
end

def make_list
  # make list
  list = []
  Dir.glob("*/").each do |d|
    cfile = File.join(d,"meta","control")
    next unless File.exist?(cfile)
    begin
      control = YAML.load_file(cfile)
      check_control(control)
      control[:dir] = d
      control[:filename] = control[:name] + "-" + control[:version] + ".jpp"
      list << control
    rescue Exception => ex
      puts "invalid meta/control in #{d}"
      puts ex
    end
  end
  list.sort!do|a,b| a[:name] <=> b[:name] end
  date = Time.now.strftime("%Y-%m-%d")
  
  str = "---\n"
  list.each do |c|
    url = @opts[:baseurl] + "/" + c[:filename]
    str << %|  - :name: "#{c[:name]}"\n|
    str << %|    :version: "#{c[:version]}"\n|
    str << %|    :vendor: "#{c[:vendor]}"\n|
    str << %|    :description: "#{c[:description].toeuc}"\n|
    str << %|    :date: "#{date}"\n|
    str << %|    :url: "#{url}"\n|
  end
  listfile = File.join(@opts[:distdir],@opts[:list])
  File.open(listfile,"w") do|io| io.puts str end
  exec("nkf -w --overwrite #{listfile}")
  sign(listfile)
  list
end

def exec(command)
  print command
  `#{command}`
  if $? == 0
    puts "  ... success"
  else
    puts "  ... error"
  end
end

def sign(file)
  if @opts[:passphrase].nil? || @opts[:passphrase].empty?
    exec("gpg -u #{@opts[:keyuser]} -abs #{file}")
  else
    exec("gpg --passphrase #{@opts[:passphrase]} -u #{@opts[:keyuser]} -abs #{file}")
  end
end

def make_package(list)
  list.each do |c|
    directory = c[:name] + "-" + c[:version]
    pkgfile = File.join(@opts[:distdir],c[:filename])
    exec("cp -rp #{c[:dir]} #{directory}")
    exec("chmod +x #{directory}/meta/*")
    exec("chmod -x #{directory}/meta/control")
    exec("chmod -x #{directory}/meta/link")
    exec("chmod -x #{directory}/meta/kentan.inc")
    exec("tar zcvf #{pkgfile} #{directory}")
    exec("rm -r #{directory}")
    sign(pkgfile)
  end
end

if $0 == __FILE__
  @opts = parse_option($0, ARGV)
  clean
  make_package(make_list)
end
