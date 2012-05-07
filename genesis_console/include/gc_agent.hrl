%% 收集的用户数据
-record(ply, {
      id,
      identity,         %% 玩家ID
      cash,             %% 现金
      credit,           %% 信用额
      update_time       %% 最后更新时间
    }).

%% 收集的代理数据
-record(agt, {
      id,               %% 代理编号
      identity,         %% 代理ID
      cash,             %% 现金
      credit,           %% 信用额
      balance,          %% 余额
      today_turnover,   %% 当日流
      week_turnover,    %% collect turnover by report
      update_time
    }).

-record(gc_agent, {
    aid,                      %%
    level,                    %% 
    parent,                   %%
    
      today_turnover,         %% init by tab_agent_daily or create empty
      today_collect_turnover, %% collect
      week_turnover,          %% init by tab_agent_daily
      week_collect_turnover,  %% collect

      %% players,          %% [ply, ...] init by tab_agent_player
      %% agents,           %% [agt, ...] init by tab_agent 

      cash,             %% amt
      credit,           %% amt

      balance,          %% amt
      players_balance,  %% amt collect
      agents_balance,   %% amt collect

      %% It is used to collect data timer
      %% to accept the data report during the timer 
      %% survival as much as possible,
      clct_t,           %% collecting timer
      clct_l            %% collecting list
    }).
