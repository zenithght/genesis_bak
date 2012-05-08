%% 收集的用户数据
-record(ply, {
    id,               %% 玩家编号
    identity,         %% 玩家标识符
    cash,             %% 现金
    credit,           %% 信用额
    update_time       %% 最后更新时间
  }).

%% 收集的代理数据
-record(agt, {
    id,               %% 代理编号
    identity,         %% 代理标识符
    cash,             %% 现金
    credit,           %% 信用额
    balance,          %% 账户余额 下级代理上报
    today_turnover,   %% 当日流水 下级代理上报
    week_turnover,    %% 当周流水 下级代理上报
    update_time       %% 最后更新时间
  }).

%% 代理进程上下文结构
-record(gc_agent, {
    id,               %% 代理编号
    identity,         %% 代理标识符
    level,            %% 代理级别（ROOT级别为0，以此类推）
    parent,           %% 上级代理标识符

    today_turnover,         %% 当日流水
    today_collect_turnover, %% 当日下级流水 下级代理上报
    week_turnover,          %% 当周流水
    week_collect_turnover,  %% 当周下级流水 下级代理上报

    cash,             %% 现金
    credit,           %% 信用额

    balance,          %% 余额
    players_balance,  %% 下级玩家余额
    agents_balance,   %% 下级代理余额

    %% 此数据用于对下级代理收集数据时使用
    clct_t,           %% 收集数据计时器
    clct_l            %% 待收集数据的代理列表
  }).
