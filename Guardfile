# A sample Guardfile
# More info at https://github.com/guard/guard#readme

guard 'haml', :input => 'src/haml', :output => 'src/view' do
  watch(/^.+(\.html\.haml)$/)
end

guard 'sass', :output => 'priv/static/css/' do
  watch %r{^src/scss/(.+\.s[ac]ss)$}
end

guard 'livereload' do
  watch(%r{priv/static/.+\.(js|css)$})
  watch(%r{src/view/.+\.html$})
end
