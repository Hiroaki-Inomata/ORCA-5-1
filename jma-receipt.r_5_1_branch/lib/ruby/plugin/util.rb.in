require "plugin"

module JMA::Plugin
  module Util

    TAR_COMMAND = "tar"
    GPG_COMMAND = "gpg"

    module_function

    def verify_sign(sign_path)
      gpg = ENV["JMA_PLUGIN_GPG"]  || GPG_COMMAND
      command = "#{gpg} --verify #{sign_path}"
      out = `#{command}`
      unless $?.exitstatus == 0
        raise "failed : #{command} #{$?.exitstatus} #{out}"
      end
    end

    def extract_tar(archive)
      dir = mktmpdir
      tar = ENV["JMA_PLUGIN_TAR"] || TAR_COMMAND
      command = "#{tar} zxf #{archive} -C #{dir}"
      out = `#{command}`
      unless $?.exitstatus == 0
        raise "failed : #{command} #{out}"
      end
      return dir
    end

    def exec_script(path,name,version,action)
      FileUtils.cd(File.dirname(path)) {
        FileUtils.chmod(0755,path)
        out = `#{path} #{name} #{version} #{action}`
        unless $?.exitstatus == 0
          raise "exec_script failure : #{path} #{$?.exitstatus} #{out}"
        end
      }
    end

    def mktmpdir
      begin
        str = ""
        10.times{
          p = [rand(128)].pack("C")
          if /[0-9a-z]/ =~ p
            str += p
          else
            redo
          end
        }
        dir = File.join(Dir.tmpdir, "jma_plugin", str)
        FileUtils.mkdir_p(dir)
        FileUtils.chmod(0700,dir)
        return dir
      rescue Errno::EEXIST
        retry
      end
    end

  end
end
