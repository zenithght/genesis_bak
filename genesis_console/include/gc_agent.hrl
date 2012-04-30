-record(ply, {         %% by db
      id,
      identity,
      cash,
      credit,
      update_time
    }).

-record(agt, {
      id,               %% by db
      identity,         %% by db
      cash,             %% by db
      credit,           %% by db
      balance,          %% by report
      today_turnover,   %% collect turnover by report
      week_turnover,    %% collect turnover by report
      update_time
    }).

-record(gc_agent, {
      today_turnover,         %% init by tab_agent_daily or create empty
      today_collect_turnover, %% collect
      week_turnover,          %% init by tab_agent_daily
      week_collect_turnover,  %% collect

      %% players,          %% [ply, ...] init by tab_agent_player
      %% agents,           %% [agt, ...] init by tab_agent 

      cash,             %% amt
      credit,           %% amt

      balance
    }).
