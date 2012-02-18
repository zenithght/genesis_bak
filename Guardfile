group :backend do
  guard 'erlang' do
    watch(/^.+\.erl$/)
  end

  guard 'eunit' do
    watch(%r{^ebin/(.+)\.beam$}) do |m| "#{m[1]}" end
  end
end

group :frontend do
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

  guard 'coffeescript', :output => 'priv/static/js/', :bare => true, :hide_success => true do
    watch(%r{src/coffee/(.+\.coffee)$})
  end
end
