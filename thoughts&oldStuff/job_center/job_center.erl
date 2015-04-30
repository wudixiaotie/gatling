-module (job_center).

-export ([init/1, handle_call/3, handle_cast/2,
          handle_info/2, terminate/2, code_change/3,
          start/0, add_job/1, work_wanted/0,
          job_done/1, job_failed/1, stop/0,
          statistics/0]).

-compile(export_all).

-behaviour (gen_server).

-include ("job_center.hrl").

-define (JOBTIME, 20).

% gen_server callbacks
init([]) ->
    JobQueue = #job_queue{},
    {ok, JobQueue}.

handle_call({in, F}, _From, #job_queue{
                                waitting = Waitting,
                                job_number = JobNumber
                            } = JobQueue) ->

    NewWaitting = queue:in({JobNumber, ?JOBTIME, F}, Waitting),
    NewJobNumber = JobNumber + 1,
    NewJobQueue = JobQueue#job_queue{
        waitting = NewWaitting,
        job_number = NewJobNumber
    },
    {reply, JobNumber, NewJobQueue};

handle_call({out}, _From, #job_queue{
                            waitting = Waitting,
                            processing = Processing
                          } = JobQueue) ->

    case queue:out(Waitting) of
        {empty, _Waitting} ->
            {reply, no, JobQueue};
        {{value, {JobNumber, JobTime, F} = JobInfo}, NewWaitting} ->
            NewProcessing = dict:store(JobNumber, {JobTime, F}, Processing),
            NewJobQueue = JobQueue#job_queue{
                waitting = NewWaitting,
                processing = NewProcessing
            },
            {reply, JobInfo, NewJobQueue}
    end;

handle_call({failed, JobNumber}, _From, #job_queue{
                                            waitting = Waitting,
                                            processing = Processing
                                          } = JobQueue) ->

    case dict:find(JobNumber, Processing) of
        error ->
            {reply, error, JobQueue};
        {ok, {JobTime, F}} ->
            NewProcessing = dict:erase(JobNumber, Processing),
            NewWaitting = queue:in_r({JobNumber, JobTime, F}, Waitting),
            NewJobQueue = JobQueue#job_queue{
                waitting = NewWaitting,
                processing = NewProcessing
            },
            {reply, ok, NewJobQueue}
    end;

handle_call({done, JobNumber}, _From, #job_queue{
                                        processing = Processing,
                                        done = Done
                                      } = JobQueue) ->

    case dict:find(JobNumber, Processing) of
        error ->
            {reply, error, JobQueue};
        {ok, {JobTime, F}} ->
            NewProcessing = dict:erase(JobNumber, Processing),
            NewDone = [{JobNumber, JobTime, F}|Done],
            NewJobQueue = JobQueue#job_queue{
                processing = NewProcessing,
                done = NewDone
            },
            {reply, ok, NewJobQueue}
    end;

handle_call(statistics, _From, #job_queue{
                                    waitting = Waitting,
                                    processing = Processing,
                                    done = Done,
                                    job_number = JobNumber
                                } = JobQueue) ->

    Statistics = {
        waitting, queue:to_list(Waitting),
        processing, dict:to_list(Processing),
        done, Done,
        job_count, JobNumber - 1
    },
    {reply, Statistics, JobQueue};

handle_call(stop, _From, JobQueue) ->
    {stop, normal, stopped, JobQueue}.



handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_info, State) -> {noreply, State}.
terminate(_Reason, _Status) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% server function
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).
add_job(F) ->
    gen_server:call(?MODULE, {in, F}).
work_wanted() ->
    gen_server:call(?MODULE, {out}).
job_failed(JobNumber) ->
    gen_server:call(?MODULE, {failed, JobNumber}).
job_done(JobNumber) ->
    gen_server:call(?MODULE, {done, JobNumber}).
statistics() ->
    gen_server:call(?MODULE, statistics).